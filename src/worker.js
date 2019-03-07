import { Elm } from './Worker.elm';
global.document = {}; // to trick parcel
const worker = Elm.Worker.init();
onmessage = ({ data }) => {
  worker.ports.data.send(data);
};
worker.ports.results.subscribe(data => {
  postMessage(data);
});
