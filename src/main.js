import { Elm } from './Main.elm';

const context = new AudioContext();

function buildNode(source) {
  const gain = context.createGain();
  const analyser = context.createAnalyser();
  gain.gain.value = 0;
  source.connect(gain);
  gain.connect(context.destination);
  source.connect(analyser);
  return { source, gain, analyser };
}

const app = Elm.Main.init({
  node: document.querySelector('main'),
});

function notePress({ id, frequency, attack, type }) {
  const osc = context.createOscillator();
  osc.frequency.value = frequency;
  const { gain, ...node } = buildNode(osc);

  gain.gain.linearRampToValueAtTime(0.5, context.currentTime + attack);
  osc.type = type;
  osc.start(0);

  app.ports.addAudioSource.send({
    id,
    node: { ...node, gain },
  });
}

function activateMic({ id }) {
  Promise.all([
    context.resume(),
    navigator.mediaDevices.getUserMedia({ audio: true }),
  ]).then(([_, stream]) => {
    const node = buildNode(context.createMediaStreamSource(stream));
    app.ports.addAudioSource.send({ id, node });
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
  notePress,
  releaseAudioSource,
  getWaveforms,
  activateMic,
};

for (const portName in subscriptions) {
  app.ports[portName].subscribe(subscriptions[portName]);
}
