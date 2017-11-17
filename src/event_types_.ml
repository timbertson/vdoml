type response = [
  | `Unhandled
  | `Handled
  | `Stop
]

type 'msg result = {
  response : response;
  message: 'msg option;
}
