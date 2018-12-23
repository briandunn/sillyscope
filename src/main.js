const { Elm } = require('./Main.elm');

const context = new AudioContext();
const notes = new Map();

const app = Elm.Main.init({
  node: document.querySelector('main'),
});

function notePress({ frequency, attack }) {
  let osc = notes.get(frequency);
  if (!osc) {
    osc = context.createOscillator();
    const gain = context.createGain();
    osc.connect(gain);
    osc.frequency.value = frequency;
    notes.set(frequency, { osc, gain });
    gain.gain.value = 0;
    gain.gain.linearRampToValueAtTime(0.5, context.currentTime + attack);
    gain.connect(context.destination);
    osc.start(0);
  }
}

function noteRelease({ frequency, attack }) {
  const note = notes.get(frequency);
  if (note) {
    note.gain.gain.linearRampToValueAtTime(0, context.currentTime + attack);
    notes.delete(frequency);
  }
}

const subscriptions = {
  notePress,
  noteRelease,
};

for (const portName in subscriptions) {
  app.ports[portName].subscribe(subscriptions[portName]);
}
