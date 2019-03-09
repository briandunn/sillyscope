import { Elm } from './Main.elm';

const context = new AudioContext();

function buildNode(source) {
  const gain = context.createGain();
  const analyser = context.createAnalyser();

  analyser.fftSize = 1 << 10;
  gain.gain.value = 0;
  source.connect(gain);
  gain.connect(context.destination);
  source.connect(analyser);
  return { source, gain, analyser };
}

function getArray(analyser, name) {
  const length = {
    getFloatTimeDomainData: analyser.fftSize,
    getFloatFrequencyDomainData: analyser.frequencyBinCount,
  };

  const list = new Float32Array(length[name]);
  analyser[name](list);
  return Array.from(list);
}

function releaseAudioSource({ release, node: { gain, source, analyser } }) {
  gain.gain.linearRampToValueAtTime(0, context.currentTime + release);
  setTimeout(() => {
    [gain, source, analyser].forEach(node => {
      node.disconnect();
    });
  }, release * 1000);
}

const analyze = (port, fn) => nodes => {
  requestAnimationFrame(() => {
    port.send(
      nodes.map(({ id, node: { analyser } }) => ({
        id,
        data: getArray(analyser, fn),
      }))
    );
  });
};

const app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: { sampleRate: context.sampleRate },
});

function notePress({ id, frequency, attack, type }) {
  const osc = context.createOscillator();
  osc.frequency.value = frequency;
  const { gain, analyser, ...node } = buildNode(osc);

  gain.gain.linearRampToValueAtTime(0.5, context.currentTime + attack);
  osc.type = type;
  osc.start(0);
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
  });
}

const getWaveforms = analyze(app.ports.waveforms, 'getFloatTimeDomainData');

const worker = new Worker('./worker.js');

worker.onmessage = ({ data }) => {
  requestAnimationFrame(() => {
    app.ports.frequencies.send(data);
  });
};

function calculateFrequencies(waveforms) {
  worker.postMessage(waveforms);
}

const subscriptions = {
  activateMic,
  getWaveforms,
  notePress,
  releaseAudioSource,
  calculateFrequencies,
};

for (const portName in subscriptions) {
  app.ports[portName].subscribe(subscriptions[portName]);
}
