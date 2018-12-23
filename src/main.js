const { Elm } = require('./Main.elm');

const context = new AudioContext();

const app = Elm.Main.init({
  node: document.querySelector('main'),
});

function notePress({ id, frequency, attack }) {
  const osc = context.createOscillator();
  const gain = context.createGain();
  const analyser = context.createAnalyser();
  analyser.fftSize = 32768;
  const waveform = new Float32Array(analyser.frequencyBinCount);
  osc.connect(gain);
  osc.frequency.value = frequency;
  gain.gain.value = 0;
  gain.gain.linearRampToValueAtTime(0.5, context.currentTime + attack);
  gain.connect(context.destination);
  osc.connect(analyser);
  osc.start(0);
  let analyserStopped = false;
  function broadcastWaveform() {
    if (!analyserStopped)
      setTimeout(
        broadcastWaveform,
        waveform.length / context.sampleRate / 1000
      );
    analyser.getFloatTimeDomainData(waveform);
    const skip = 16;
    const waveformArray = Array.from(waveform);
    const samples = [];
    for (var i = 0; i < analyser.frequencyBinCount; i += skip) {
      const sample = waveformArray[i];
      if (sample) samples.push(sample);
    }
    app.ports.waveform.send({ waveform: samples, id });
  }
  broadcastWaveform();

  app.ports.notePressed.send({
    id,
    frequency,
    attack,
    node: {
      osc,
      gain,
      stopAnalyzer: () => {
        analyserStopped = true;
      },
    },
  });
}

function noteRelease({ attack, node: { gain, stopAnalyzer } }) {
  gain.gain.linearRampToValueAtTime(0, context.currentTime + attack);
  setTimeout(() => {
    gain.disconnect();
    stopAnalyzer();
  }, attack * 1000);
}

const subscriptions = {
  notePress,
  noteRelease,
};

for (const portName in subscriptions) {
  app.ports[portName].subscribe(subscriptions[portName]);
}
