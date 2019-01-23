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

navigator.mediaDevices.getUserMedia({ audio: true }).then(stream => {
  const node = buildNode(context.createMediaStreamSource(stream));
  app.ports.notePressed.send({ id: 777, node });
});

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

  app.ports.notePressed.send({
    id,
    frequency,
    attack,
    node: { ...node, gain },
  });
}

function noteRelease({ release, node: { gain, source, analyser } }) {
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
  noteRelease,
  getWaveforms,
};

for (const portName in subscriptions) {
  app.ports[portName].subscribe(subscriptions[portName]);
}
