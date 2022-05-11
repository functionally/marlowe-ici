
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
import { encode, decode } from "@ipld/dag-cbor"
import { WebRTCStar     } from "libp2p-webrtc-star"


const filters = require('libp2p-websockets/src/filters')

const transportKey = WS.prototype[Symbol.toStringTag]


export const topic = "marlowe-ici"

const ipfsInBrowser = true

export const home = "/ip4/127.0.0.1/tcp/5001"

export const homes = [
  "/ip4/127.0.0.1/tcp/4011/ws/p2p/QmSCkKmKjYmPVQGaSRFRjKG28xz5iK3zPK6uCcEE4nxMPB",       // oryx ipfs
  "/ip4/127.0.0.1/tcp/4003/ws/p2p/12D3KooWDur2A4JM46Kcay71FgVMjRmgHsvN5iFswiHghCtoQYHA", // oryx relay
//"/ip4/192.168.86.42/tcp/4011/ws/p2p/QmeTpDf65kim2LsRDcRRWmsGkqZWqMB2GzfrPpckN1vuym",   // gazelle ipfs
]


async function connectHomes() {
  Promise.all(homes.map(address => ipfs.swarm.connect(address)))
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
    uiIndexCid.innerHTML = "<a href='http://127.0.0.1:5001/ipfs/bafybeihcyruaeza7uyjd6ugicbcrqumejf6uf353e5etdkhotqffwtguva/#/explore/ipfs/" + tip.CID + "' target='_blank'>" + tip.CID + "</a>"
    if (tip.addresses.length > 0)
//    uiMarloweEvent.innerText = JSON.stringify(tip.addresses, null, 2)
      uiMarloweEvent.prepend(renderjson(tip.addresses[0]))
  } else
    console.warn("Unexpected message on \"marlowe-ici\" topic.", msg.data)
}

export async function subscribe() {
  await ipfs.pubsub.subscribe(topic, updateTip)
}


export let ipfs = null

export async function initialize() {

  if (ipfsInBrowser)
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
              // FIXME: Remove to limit to DNS or WSS.
              filter: filters.all
//            filter: filters.dnsWss
//            filter: filters.dnsWsOrWss
            }
          },
          pubsub: {
            enabled: true
          }
        }
      },
      config: {
        Addresses: {
          Bootstrap: [
          ],
          Swarm: [
            "/ip4/127.0.0.1/tcp/9090/wss/p2p-webrtc-star",
//          "/dns4/wrtc-star1.par.dwebops.pub/tcp/443/wss/p2p-webrtc-star",
//          "/dns4/wrtc-star2.sjc.dwebops.pub/tcp/443/wss/p2p-webrtc-star",
          ]
        }
      },
      relay: {
        enabled: true,
        hop: {
          enabled: true
        }
      }
    })
  else
    ipfs = CLIENT.create(home)

  const info = await ipfs.id()
  uiId.innerText = info.id
  uiAddresses.innerHTML = "<ul>" + info.addresses.map(address => "<li class='pre'>" + address + "</li>").join("") + "</ul>"

  setInterval(async () => {
    const peers = await ipfs.swarm.peers()
    uiPeers.innerHTML = "<ul>" + peers.reverse().map(peer => "<li class='pre'>" + (peer.addr.toString().indexOf('/p2p/') >= 0 ? peer.addr : peer.addr + "/p2p/" + peer.peer) + "</li>").join("") + "</ul>"
  }, 5000)

  await connectHomes()
  setInterval(async () => { await connectHomes() }, 150000)

  subscribe()

}
