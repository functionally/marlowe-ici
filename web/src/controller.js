
'use strict'


import * as IPFS       from "ipfs-core"
import * as CLIENT     from "ipfs-http-client"
import * as WS         from "libp2p-websockets"
import * as renderjson from "renderjson"

import { CID            } from "multiformats/cid"
import { MPLEX          } from 'libp2p-mplex'
import { Multiaddr      } from "multiaddr"
import { NOISE          } from 'libp2p-noise'
import { WebSockets     } from 'libp2p-websockets'
// import { encode, decode } from "@ipld/dag-cbor"
import { WebRTCStar     } from "libp2p-webrtc-star"


const filters = require('libp2p-websockets/src/filters')

const transportKey = WS.prototype[Symbol.toStringTag]


export let topic = "marlowe-ici"

export const MODE_SERVE_LOCAL   = 0
export const MODE_BROWSE_LOCAL  = 1
export const MODE_BROWSE_STATIC = 2

export const mode = MODE_BROWSE_STATIC

const home = "/ip4/127.0.0.1/tcp/5001"

const homes = [
  [ // MODE_SERVE_LOCAL
  ],
  [ // MODE_BROWSE_LOCAL
    "/ip4/127.0.0.1/tcp/4011/ws/p2p/QmSCkKmKjYmPVQGaSRFRjKG28xz5iK3zPK6uCcEE4nxMPB"      , // oryx ipfs
    "/ip4/127.0.0.1/tcp/4003/ws/p2p/12D3KooWDur2A4JM46Kcay71FgVMjRmgHsvN5iFswiHghCtoQYHA", // oryx relay
    "/ip4/192.168.86.42/tcp/4011/ws/p2p/QmeTpDf65kim2LsRDcRRWmsGkqZWqMB2GzfrPpckN1vuym"  , // gazelle ipfs
  ],
  [ // MODE_BROWSE_STATIC
    "/dns4/substrate.functionally.dev/tcp/4008/wss/p2p/12D3KooWAX1YJxFMBvvayA8d7adVnieKqcqEYhEJTG1gQghUJt8h",
  ],
]

const swarms = [
  [ // MODE_SERVE_LOCAL
  ],
  [ // MODE_BROWSE_LOCAL
    "/dns4/substrate.functionally.dev/tcp/4009/wss/p2p-webrtc-star/",
    "/dns4/substrate.functionally.dev/tcp/4005/ws/p2p-webrtc-star/" ,
    "/ip4/127.0.0.1/tcp/4005/wss/p2p-webrtc-star"                   ,
  ],
  [ // MODE_BROWSE_STATIC
    "/dns4/substrate.functionally.dev/tcp/4009/wss/p2p-webrtc-star/",
    "/dns4/wrtc-star1.par.dwebops.pub/tcp/443/wss/p2p-webrtc-star"  ,
    "/dns4/wrtc-star2.sjc.dwebops.pub/tcp/443/wss/p2p-webrtc-star"  ,
  ]
]

const modes = [
  filters.all         , // MODE_SERVE_LOCAL
  filters.dnsWsOrWss  , // MODE_BROWSE_LOCAL
  filters.dnsWss      , // MODE_BROWSE_STATIC
]


async function connectHomes() {
  Promise.all(homes[mode].map(address => ipfs.swarm.connect(address)))
}

export function makeCID(cid) {
  return CID.parse(cid)
}

export function makeMultiaddr(address) {
  return new Multiaddr(address)
}


export async function connect(address) {
  const result = await ipfs.swarm.connect(address)
  console.info(result)
}


export let tip = null

const decoder = new TextDecoder()

function updateTip(msg) {
  tip = JSON.parse(decoder.decode(msg.data))
  if (tip.slot) {
    uiSlotNo.innerText = tip.slot
    uiBlockNo.innerText = tip.block
    uiBlockHash.innerText = tip.hash
    uiIndexCid.innerHTML = "<a href='https://substrate.functionally.dev:4010/ipfs/bafybeihcyruaeza7uyjd6ugicbcrqumejf6uf353e5etdkhotqffwtguva/#/explore/ipfs/" + tip.CID + "' target='marlowe-ici'>" + tip.CID + "</a>"
    if (tip.latest.length > 0)
      uiMarloweEvent.prepend(renderjson(tip.latest[0]))
    const certificate = JSON.stringify(tip.certificate)
    uiCertificate.innerText = certificate.slice(0, 30) + "....." + certificate.slice(-30)
    uiClipboard.value = certificate
  } else
    console.warn("Unexpected message on \"" + topic + "\" topic.", msg.data)
}

export function copyCertificate() {
  uiClipboard.style.display = "inline"
  uiClipboard.select()
  document.execCommand("copy")
  uiClipboard.style.display = "none"
}

export async function subscribe() {
  await ipfs.pubsub.subscribe(topic, updateTip)
}


export let ipfs = null

export async function initialize(theTopic) {

  topic = theTopic
  uiTopic.innerText = topic

  renderjson.set_icons('⊞', '⊟')

  if (mode == MODE_SERVE_LOCAL)
    ipfs = CLIENT.create(home)
  else
    ipfs = await IPFS.create({
      preload: {
        enabled: false
      },
      modules: {
        transport: [WebSockets, WebRTCStar],
        streamMuxer: [MPLEX],
        connEncryption: [NOISE]
      },
      repo: 'marlowe-ici-' + Math.random(),
      libp2p: {
        config: {
          peerDiscovery: {
            autoDial: true,
            webRTCStar: {
              enabled: true
            }
          },
          transport: {
            [transportKey]: {
              filter: modes[mode]
            }
          },
          pubsub: {
            enabled: true
          }
        }
      },
      config: {
        Addresses: {
          Bootstrap: [],
          Swarm: swarms[mode]
        }
      },
      relay: {
        enabled: true,
        hop: {
          enabled: true
        }
      }
    })

  const info = await ipfs.id()
  uiId.innerText = info.id
  uiAddresses.innerHTML = "<ul>" + info.addresses.map(address => "<li class='pre'>" + address + "</li>").join("") + "</ul>"

  setInterval(async () => {
    const peers = await ipfs.swarm.peers()
    uiPeers.innerHTML = "<ul>" + peers.reverse().map(peer => "<li class='pre'>" + (peer.addr.toString().indexOf('/p2p/') >= 0 ? peer.addr : peer.addr + "/p2p/" + peer.peer) + "</li>").join("") + "</ul>"
  }, 5000)

  setTimeout( async () => { await connectHomes() },   5000)
  setInterval(async () => { await connectHomes() }, 150000)

  subscribe()

}
