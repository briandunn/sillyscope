import { Elm } from './Main.elm';

const context = new AudioContext();

const app = Elm.Main.init({
  node: document.querySelector('main'),
});

function notePress({ id, frequency, attack, type }) {
  const osc = context.createOscillator();
  const gain = context.createGain();
  const analyser = context.createAnalyser();
  analyser.fftSize = 4096;
  osc.connect(gain);
  osc.frequency.value = frequency;
  gain.gain.value = 0;
  gain.gain.linearRampToValueAtTime(0.5, context.currentTime + attack);
  gain.connect(context.destination);
  osc.connect(analyser);
  osc.type = type;
  osc.start(0);

  app.ports.notePressed.send({
    id,
    frequency,
    attack,
    node: {
      analyser,
      gain,
      osc,
    },
  });
}

function noteRelease({ attack, node: { osc, analyser, gain } }) {
  gain.gain.linearRampToValueAtTime(0, context.currentTime + attack);
  setTimeout(() => {
    [osc, gain, analyser].forEach(node => node.disconnect());
  }, attack * 1000);
}

function getWaveforms(notes) {
  requestAnimationFrame(() => {
    const waveforms = notes.map(({ id, node: { analyser } }) => {
      const waveform = new Float32Array(analyser.frequencyBinCount);
      analyser.getFloatTimeDomainData(waveform);
      return { id, waveform: Array.from(waveform) };
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
