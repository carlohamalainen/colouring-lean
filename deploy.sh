#!/bin/sh
set -eu
cd "$(dirname "$0")"
ansible-playbook -i inventory.yml deploy.yml
