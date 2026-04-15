#!/bin/bash
# Validate package.json against the official Fresh package schema
#
# Prerequisite: pip install jsonschema
curl -sSL https://raw.githubusercontent.com/sinelaw/fresh/main/scripts/validate-package.sh | bash
