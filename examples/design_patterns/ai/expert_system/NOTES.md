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
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________
-->

Design pattern:
	Expert system

Description:
	Allow representation of specialized domain knowledge that can
	be used to solve problems in that domain. These problems are
	typically classification problems.

Related examples:
	The `examples/birds` example implements a bird identification
	expert system. The `examples/lpa/faults` implements an expert
	system for diagnosing automobile faults.

This design pattern is usually implemented using some representation of
rules that express the domain knowledge and a meta-interpreter for those
rules that allows asking (usually, interactively) questions about rule
premisses. Answers to those questions are stored to avoid repeating the
questions and an allow using the answers when attempting to apply other
rules that share some of the premisses.
