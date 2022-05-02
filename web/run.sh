#!/usr/bin/env bash

set -ve

export DEBUG='express:*'
npx webpack
npx webpack-dev-server
