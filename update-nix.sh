#!/bin/bash

if [[ -n $(git status --porcelain) ]]; then
	echo "Git is dirty, aborting"
	exit 1
fi

if [[ ! -f "flake.nix" ]]; then
	echo "Flake.nix file does not exist"
	exit 1
fi

echo "Updating Flake lock: "
nix flake update --commit-lock-file --commit-lockfile-summary "flake.lock: Update"

echo "Building Home-manager configuration: "
home-manager build --flake .

echo "Activating home-manager configuration: "
home-manager switch --flake .

echo "Pushing flake.lock"
git push
