---
jupyter:
  jupytext:
    formats: md:myst
    text_representation:
      extension: .md
      format_name: myst
      format_version: '0.13'
      jupytext_version: 1.16.4
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0
-->

# Yoda server

This example defines a small OpenAI-compatible HTTP server using the
`open_ai` library server facade. The backend accepts a chat-style request body
with exactly one user message and replies with a minimal `choices` array whose
assistant message contains a lower-case Yoda-style version of the sentence.

The server supports both `POST /chat/completions` and `POST /responses`. For
simplicity, both endpoints use the same `messages` request shape.

Load the example with:

```logtalk
| ?- logtalk_load(yoda_server(loader)).
```

Start a local server that serves two client connections:

```logtalk
| ?- yoda_server::serve(8080, 2).
```

In another shell, send a chat completion request:

```shell
curl -sS \
  -H 'Content-Type: application/json' \
  --data-binary '{"model":"yoda","messages":[{"role":"user","content":"You will learn patience"}]}' \
  http://127.0.0.1:8080/chat/completions
```

The response contains:

```json
{"choices":[{"message":{"content":"learn patience will you"}}]}
```

The `/responses` endpoint accepts the same request body:

```shell
curl -sS \
  -H 'Content-Type: application/json' \
  --data-binary '{"model":"yoda","messages":[{"role":"user","content":"I am hungry"}]}' \
  http://127.0.0.1:8080/responses
```

The response contains:

```json
{"choices":[{"message":{"content":"hungry am i"}}]}
```

The example assumes the user message is a single sentence. The sentence is
lower-cased, split into words using `atom::split/3`, transformed with
`yoda_words/2`, and joined using `atomic_list_concat/3`. Punctuation,
streaming, model selection, persisted completions, tool calls, and the native
Responses API `input`/`output_text` shape are intentionally outside the scope of
this example.
