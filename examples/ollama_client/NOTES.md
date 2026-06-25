---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an AS IS BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________
-->

# ollama_client

This example shows how to use the `open_ai` client facade with a locally
running Ollama server. It uses Ollama's OpenAI-compatible endpoint:

```text
http://127.0.0.1:11434/v1
```

Start Ollama in a separate terminal:

```text
ollama serve
```

Pull a small model if you do not already have one installed:

```text
ollama pull llama3.2
```

Load the example with:

```logtalk
logtalk_load(ollama_client(loader)).
```

Check if the local server is reachable:

```logtalk
ollama_client::available.
```

List local models:

```logtalk
ollama_client::models(Models).
```

Ask a short question using one of the returned model identifiers:

```logtalk
ollama_client::ask('llama3.2:latest', 'Say OK.', Answer).
```

The example sends OpenAI-format requests through `open_ai_client::request/5`.
It does not call Ollama's native `/api/*` endpoints.

The unit tests include deterministic tests using canned OpenAI-compatible
responses. The live availability test is conditional on a reachable local
server. The live prompt tests use `OLLAMA_CLIENT_TEST_MODEL` when it names an
installed local model; otherwise, they look for a known chat model in the
local model list.

Use `OLLAMA_CLIENT_TEST_MODEL` to force a specific model. For example:

```text
OLLAMA_CLIENT_TEST_MODEL=llama3.2:latest logtalk_tester -p swi
```
