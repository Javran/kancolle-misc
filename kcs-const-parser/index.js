import fs from 'fs'
import safeEval from 'safe-eval'

/*
  Reads and parses kcs_const.js and output as JSON.

  Usage: node index.js <input>

  where <input> is case sensitive:

  - `def`: default gadget url
  - `stdin`
  - (TODO): file and custom url sources are certainly possible but not necessary for now.

  For now always outputs to stdout. Verify by checking exit code = 0 and output JSON being an Object.

 */
let raw
const rawSource = process.argv[2]

if (rawSource === 'def') {
  const resp = await fetch('https://w00g.kancolle-server.com/gadget_html5/js/kcs_const.js')
  if (!resp.ok) {
    throw new Error(resp);
  }
  raw = await resp.text()
} else if (rawSource === 'stdin') {
  raw = fs.readFileSync(0, 'utf-8')
} else {
  throw new Error(`invalid source spec: ${rawSource}`)
}

const body = `(() => {
  ${raw};
  return JSON.stringify({ConstGadgetInfo, ConstServerInfo, ConstURLInfo, MaintenanceInfo, VersionInfo})
})()`

console.log(safeEval(body))
