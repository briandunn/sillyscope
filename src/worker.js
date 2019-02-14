import { Elm } from './Worker.elm';
global.document = {}; // to trick parcel
const worker = Elm.Worker.init();
onmessage = ({ data }) => {
  console.log('onmessage');
  worker.ports.data.send(data);
};
worker.ports.results.subscribe((...args) => {
  console.log(args);
  postMessage(args);
});
