import { Elm } from './Worker.elm';
global.document = {}; // to trick parcel
const worker = Elm.Worker.init();
onmessage = ({ data: { payload } }) => {
  worker.ports.data.send(payload);
};
worker.ports.results.subscribe((...args) => {
  postMessage(args);
});
