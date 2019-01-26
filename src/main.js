import { Elm } from './Main.elm';

const context = new AudioContext();

function buildNode(source) {
  const gain = context.createGain();
  const analyser = context.createAnalyser();
  analyser.fftSize = 4096;
  gain.gain.value = 0;
  source.connect(gain);
  gain.connect(context.destination);
  source.connect(analyser);
  return { source, gain, analyser };
}

function getFft(analyser) {
  const list = new Float32Array(analyser.frequencyBinCount);
  analyser.getFloatFrequencyData(list);
  return list;
}

function dominantFreq(analyzer) {
  const list = getFft(analyzer);
  return (
    list.reduce(([max, j], x, i) => (max < x ? [x, i] : [max, j]), [
      -1000,
      -1000,
    ]) / list.length
  );
}

const app = Elm.Main.init({
  node: document.querySelector('main'),
});

function notePress({ id, frequency, attack, type }) {
  const osc = context.createOscillator();
  osc.frequency.value = frequency;
  const { gain, analyser, ...node } = buildNode(osc);

  gain.gain.linearRampToValueAtTime(0.5, context.currentTime + attack);
  osc.type = type;
  osc.start(0);

  app.ports.addDominantFreq.send({ id, freq: dominantFreq(analyser) });
  app.ports.addAudioSource.send({
    id,
    node: { gain, analyser, ...node },
  });
}

function activateMic({ id }) {
  Promise.all([
    context.resume(),
    navigator.mediaDevices.getUserMedia({ audio: true }),
  ]).then(([_, stream]) => {
    const node = buildNode(context.createMediaStreamSource(stream));
    app.ports.addAudioSource.send({ id, node });
    getFfts([{ id, node }]); // seems a bit like cheatin
  });
}

function releaseAudioSource({ release, node: { gain, source, analyser } }) {
  gain.gain.linearRampToValueAtTime(0, context.currentTime + release);
  setTimeout(() => {
    [gain, source, analyser].forEach(node => {
      node.disconnect();
    });
  }, release * 1000);
}

function getFfts(nodes) {
  requestAnimationFrame(() => {
    app.ports.ffts.send(
      nodes.map(({ id, node: { analyser } }) => ({
        id,
        data: Array.from(getFft(analyser)),
      }))
    );
  });
}

function getWaveforms(notes) {
  requestAnimationFrame(() => {
    const waveforms = notes.map(({ id, node: { analyser } }) => {
      const waveform = new Float32Array(analyser.frequencyBinCount);
      analyser.getFloatTimeDomainData(waveform);
      return { id, data: Array.from(waveform) };
    });

    app.ports.waveforms.send(waveforms);
  });
}

const subscriptions = {
  activateMic,
  getFfts,
  getWaveforms,
  notePress,
  releaseAudioSource,
};

for (const portName in subscriptions) {
  app.ports[portName].subscribe(subscriptions[portName]);
}
