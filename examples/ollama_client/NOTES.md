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

Check if the local server is reachable (assumes the default `http://127.0.0.1:11434/v1` URL):

```logtalk
ollama_client::available.
```

List local chat-capable models:

```logtalk
ollama_client::models(Models).
```

Ask a short question using one of the returned model identifiers:

```logtalk
ollama_client::ask('llama3.2:latest', 'Say OK.', Answer).
```

If your Ollama installation uses a non-default URL, the `ollama_client` object
provides also `available/1`, `models/2`, and `ask/4` predicates that take a
list of options, including a `base_url(URL)` option to override the default
`http://127.0.0.1:11434/v1` URL.

The example lists installed models using Ollama's OpenAI-compatible `/v1/models`
endpoint and then calls Ollama's native `/api/show` endpoint once per model to
filter the result to chat-capable models. Current Ollama releases report text
generation models using the `completion` capability, which this example treats
as chat-capable. Chat requests themselves still use `open_ai_client::request/5`,
and they set `reasoning_effort` to `none` so reasoning-capable models return a
final short answer instead of spending the small token budget on a reasoning
trace.

The unit tests include deterministic tests using canned OpenAI-compatible and
native Ollama responses. The live availability test is conditional on a
reachable local server. The live prompt tests use `OLLAMA_CLIENT_TEST_MODEL`
when it names an installed chat-capable model; otherwise, they use the first
chat-capable model returned by `ollama_client::models/1`.

Use `OLLAMA_CLIENT_TEST_MODEL` to force a specific model. For example:

```text
OLLAMA_CLIENT_TEST_MODEL=llama3.2:latest logtalk_tester -p swi
```
