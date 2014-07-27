#!/bin/sh

([[ -f server/breathe.sh ]] && echo "+ server alive") || echo "- no server"
([[ -f client/client.sh ]] && echo "+ client alive") || echo "- no client"
([[ -f observer/observer.sh ]] && echo "+ observer alive") || echo "- no server"
