#!/bin/bash

if ! command -v jq &> /dev/null; then
    echo "Error: jq is not installed. Please install it using:"
    echo "  sudo apt-get install jq"
    exit 1
fi

if ! command -v sed &> /dev/null; then
  echo "Error: sed is not installed. Please install it using:"
  echo "   sudo apt-get install sed"
  exit 1
fi

latest_release_tag=$(curl -s https://api.github.com/repos/10Pines/pdepreludat/releases/latest | jq -r '.tag_name')

if [ -z "$latest_release_tag" ]; then
    echo "Error: Failed to extract latest release tag"
    exit 1
fi

latest_commit=$(curl -s https://api.github.com/repos/10Pines/pdepreludat/git/ref/tags/${latest_release_tag} | jq -r '.object.sha')

if [ -z "$latest_commit" ]; then
    echo "Error: Failed to extract commit SHA"
    exit 1
fi
echo "Fetched last commit."

FILE="stack.yaml"
if [ ! -f "$FILE" ]; then
  echo "Error: File: $FILE does not exist."
  exit 1
fi

sed -i "s/commit: *.*/commit: $latest_commit/g" "$FILE"
if [ $? -ne 0 ]; then
  echo "Error: Failed to update commit SHA in $FILE"
  exit 1
fi
echo "Updated commit SHA in $FILE"

stack_yaml_data=$(curl -H "Accept: application/vnd.github.raw" https://api.github.com/repos/10Pines/pdepreludat/contents/stack.yaml)

if [ -z "$stack_yaml_data" ]; then
  echo "Error: Failed to obtain stack.yaml data"
  exit 1
fi

resolver=$(sed -n '/^resolver/p' <<< "$stack_yaml_data")

if [ -z "$resolver" ]; then
  echo "Error: Failed to extract resolver information"
  exit 1
fi

echo "Fetched resolver: $resolver"

sed -i "s/resolver: *.*/$resolver/g" "$FILE"
if [ $? -ne 0 ]; then
  echo "Error: Failed to update resolver in $FILE"
  exit 1
fi

echo "stack.yaml updated"
