IPLD Chain Index for Marlowe
============================

This application is a decentralized, distributed chain index for [Marlowe](https://marlowe-finance.io/) transactions on the [Cardano blockchain](https://cardano.org/).

The application is deployed on [IPFS](https://ipfs.io/) at <[ipns://k51qzi5uqu5dlg4r3olegweltx6f64rlietex6kmfl4kq00heyqvwhrbaofbg0](https://substrate.functionally.dev:4010/ipns/k51qzi5uqu5dlg4r3olegweltx6f64rlietex6kmfl4kq00heyqvwhrbaofbg0)> and on GitHub Pages at <[https://marlowe-ici.functionally.io](https://marlowe-ici.functionally.io/)>. The application may be accessed through any [IPFS gateway](https://ipfs.github.io/public-gateway-checker/), but fetching the content over the IPFS network may take some time.


Indexer
-------

The indexer is a Haskell chain-sync client that watches the blockchain for transactions relevant to Marlowe contracts. It indexes those transactions as an [IPLD](https://ipld.io/) Merkle DAG and publishes the root of the indices to a [IPFS Pub/Sub](https://docs.libp2p.io/concepts/publish-subscribe/) topic. The indexing is deterministic, so any indexer will produce the same sequence of IPLD [CIDs](https://docs.ipfs.io/concepts/content-addressing/) as any other indexer.

Each root CID is signed by the [Atala PRISM](https://atalaprism.io/app) decentralized identifier ([DID](https://w3c-ccg.github.io/did-primer/)) for the application, <code>did:prism:325526fea90b89149c77901340dce22e4cc56976cf6fbecb598f284f3bbaecf4</code>. Using CIDs ensures the integrity of content, and having the root CID signed in a DID certificate attests to the authenticity of the index.


Web Application
---------------

The JavaScript web application subscribes to the pub/sub topic for the Marlowe index and displays the Marlowe transactions in real-time along with the CID for the current Marlowe index. The application is serverless (i.e., no backend) and discovers the content of the Marlowe index and pub/sub topic using the [WebRTC-Star](https://github.com/libp2p/js-libp2p-webrtc-star#readme) transport and signalling of [libp2p](https://libp2p.io/) via a bootstrapping process.


Status and Documentation
------------------------

This application is a work in progress. Documentation on the message and index schemas has yet to be written.
