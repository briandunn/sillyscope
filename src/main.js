const { Elm } = require('./Main.elm');

const context = new AudioContext();
const notes = new Map();

const app = Elm.Main.init({
  node: document.querySelector('main'),
});

function notePress(freq) {
  let osc = notes.get(freq);
  if (!osc) {
    osc = context.createOscillator();
    const gain = context.createGain();
    osc.connect(gain);
    osc.frequency.value = freq;
    notes.set(freq, osc);
    gain.gain.value = 0.5;
    gain.connect(context.destination);
    osc.start(0);
  }
}

function noteRelease(freq) {
  const osc = notes.get(freq);
  if (osc) {
    osc.stop();
    notes.delete(freq);
  }
}

const subscriptions = {
  notePress,
  noteRelease,
};

for (const portName in subscriptions) {
  app.ports[portName].subscribe(subscriptions[portName]);
}
