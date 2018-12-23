const { Elm } = require('./Main.elm');

const context = new AudioContext();

const app = Elm.Main.init({
  node: document.querySelector('main'),
});

function notePress({ id, frequency, attack }) {
  const osc = context.createOscillator();
  const gain = context.createGain();

  osc.connect(gain);
  osc.frequency.value = frequency;
  app.ports.notePressed.send({ id, frequency, attack, node: { osc, gain } });
  gain.gain.value = 0;
  gain.gain.linearRampToValueAtTime(0.5, context.currentTime + attack);
  gain.connect(context.destination);
  osc.start(0);
}

function noteRelease(args) {
  console.log(args);

  const {
    attack,
    node: { gain },
  } = args;
  if (gain) {
    gain.gain.linearRampToValueAtTime(0, context.currentTime + attack);
    setTimeout(() => {
      gain.disconnect();
    }, attack * 1000);
  }
}

const subscriptions = {
  notePress,
  noteRelease,
};

for (const portName in subscriptions) {
  app.ports[portName].subscribe(subscriptions[portName]);
}
