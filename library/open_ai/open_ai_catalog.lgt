%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(open_ai_catalog).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Generated catalog for the public non-admin OpenAI API endpoint surface.'
	]).

	:- public(spec_version/1).
	:- mode(spec_version(-atom), one).
	:- info(spec_version/1, [
		comment is 'Returns the pinned OpenAI OpenAPI document version used to generate the catalog.',
		argnames is ['Version']
	]).

	:- public(operation_count/1).
	:- mode(operation_count(-integer), one).
	:- info(operation_count/1, [
		comment is 'Returns the number of public non-admin OpenAI operations in the generated catalog.',
		argnames is ['Count']
	]).

	:- public(operation/8).
	:- mode(operation(?atom, ?atom, ?atom, ?atom, ?atom, ?list(compound), ?compound, ?list(compound)), zero_or_more).
	:- info(operation/8, [
		comment is 'Enumerates generated OpenAI operation descriptors.',
		argnames is ['Id', 'Tag', 'Method', 'Path', 'Summary', 'Parameters', 'RequestBody', 'Responses']
	]).

	:- public(operation_properties/2).
	:- mode(operation_properties(?atom, ?list(compound)), zero_or_more).
	:- info(operation_properties/2, [
		comment is 'Enumerates additional generated OpenAPI properties for an operation.',
		argnames is ['Id', 'Properties']
	]).

	spec_version('2.3.0').

	operation_count(161).

	operation(listAssistants, 'Assistants', get, '/assistants', 'Returns a list of assistants.', [], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createAssistant, 'Assistants', post, '/assistants', 'Create an assistant with a model and instructions.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteAssistant, 'Assistants', delete, '/assistants/{assistant_id}', 'Delete an assistant.', [parameter(assistant_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(getAssistant, 'Assistants', get, '/assistants/{assistant_id}', 'Retrieves an assistant.', [parameter(assistant_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(modifyAssistant, 'Assistants', post, '/assistants/{assistant_id}', 'Modifies an assistant.', [parameter(assistant_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(createThread, 'Assistants', post, '/threads', 'Create a thread.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(createThreadAndRun, 'Assistants', post, '/threads/runs', 'Create a thread and run it in one request.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteThread, 'Assistants', delete, '/threads/{thread_id}', 'Delete a thread.', [parameter(thread_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(getThread, 'Assistants', get, '/threads/{thread_id}', 'Retrieves a thread.', [parameter(thread_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(modifyThread, 'Assistants', post, '/threads/{thread_id}', 'Modifies a thread.', [parameter(thread_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(listMessages, 'Assistants', get, '/threads/{thread_id}/messages', 'Returns a list of messages for a given thread.', [parameter(thread_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createMessage, 'Assistants', post, '/threads/{thread_id}/messages', 'Create a message.', [parameter(thread_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteMessage, 'Assistants', delete, '/threads/{thread_id}/messages/{message_id}', 'Deletes a message.', [parameter(thread_id, path, 'Path parameter.', true, @true), parameter(message_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(getMessage, 'Assistants', get, '/threads/{thread_id}/messages/{message_id}', 'Retrieve a message.', [parameter(thread_id, path, 'Path parameter.', true, @true), parameter(message_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(modifyMessage, 'Assistants', post, '/threads/{thread_id}/messages/{message_id}', 'Modifies a message.', [parameter(thread_id, path, 'Path parameter.', true, @true), parameter(message_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(listRuns, 'Assistants', get, '/threads/{thread_id}/runs', 'Returns a list of runs belonging to a thread.', [parameter(thread_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createRun, 'Assistants', post, '/threads/{thread_id}/runs', 'Create a run.', [parameter(thread_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(getRun, 'Assistants', get, '/threads/{thread_id}/runs/{run_id}', 'Retrieves a run.', [parameter(thread_id, path, 'Path parameter.', true, @true), parameter(run_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(modifyRun, 'Assistants', post, '/threads/{thread_id}/runs/{run_id}', 'Modifies a run.', [parameter(thread_id, path, 'Path parameter.', true, @true), parameter(run_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(cancelRun, 'Assistants', post, '/threads/{thread_id}/runs/{run_id}/cancel', 'Cancels a run that is `in_progress`.', [parameter(thread_id, path, 'Path parameter.', true, @true), parameter(run_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(listRunSteps, 'Assistants', get, '/threads/{thread_id}/runs/{run_id}/steps', 'Returns a list of run steps belonging to a run.', [parameter(thread_id, path, 'Path parameter.', true, @true), parameter(run_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(getRunStep, 'Assistants', get, '/threads/{thread_id}/runs/{run_id}/steps/{step_id}', 'Retrieves a run step.', [parameter(thread_id, path, 'Path parameter.', true, @true), parameter(run_id, path, 'Path parameter.', true, @true), parameter(step_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(submitToolOuputsToRun, 'Assistants', post, '/threads/{thread_id}/runs/{run_id}/submit_tool_outputs', 'When a run has the `status: "requires_action"` and `required_action.type` is `submit_tool_outputs`, this endpoint can be used to submit the outputs from the tool calls once they''re all completed. All outputs must be submitted in a single request.', [parameter(thread_id, path, 'Path parameter.', true, @true), parameter(run_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(createSpeech, 'Audio', post, '/audio/speech', 'Generates audio from the input text. Returns the audio file content, or a stream of audio events.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(createTranscription, 'Audio', post, '/audio/transcriptions', 'Transcribes audio into the input language. Returns a transcription object in `json`, `diarized_json`, or `verbose_json` format, or a stream of transcript events.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(createTranslation, 'Audio', post, '/audio/translations', 'Translates audio into English.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(listVoiceConsents, 'Audio', get, '/audio/voice_consents', 'Returns a list of voice consent recordings.', [], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createVoiceConsent, 'Audio', post, '/audio/voice_consents', 'Upload a voice consent recording.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteVoiceConsent, 'Audio', delete, '/audio/voice_consents/{consent_id}', 'Deletes a voice consent recording.', [parameter(consent_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(getVoiceConsent, 'Audio', get, '/audio/voice_consents/{consent_id}', 'Retrieves a voice consent recording.', [parameter(consent_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(updateVoiceConsent, 'Audio', post, '/audio/voice_consents/{consent_id}', 'Updates a voice consent recording (metadata only).', [parameter(consent_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(createVoice, 'Audio', post, '/audio/voices', 'Creates a custom voice.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(listBatches, 'Batch', get, '/batches', 'List your organization''s batches.', [], none, [response(200, 'Batch listed successfully.', [media('application/json', @true)])]).
	operation(createBatch, 'Batch', post, '/batches', 'Creates and executes a batch from an uploaded file of requests', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Batch created successfully.', [media('application/json', @true)])]).
	operation(retrieveBatch, 'Batch', get, '/batches/{batch_id}', 'Retrieves a batch.', [parameter(batch_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Batch retrieved successfully.', [media('application/json', @true)])]).
	operation(cancelBatch, 'Batch', post, '/batches/{batch_id}/cancel', 'Cancels an in-progress batch. The batch will be in status `cancelling` for up to 10 minutes, before changing to `cancelled`, where it will have partial results (if any) available in the output file.', [parameter(batch_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Batch is cancelling. Returns the cancelling batch''s details.', [media('application/json', @true)])]).
	operation(listChatCompletions, 'Chat', get, '/chat/completions', 'List stored Chat Completions. Only Chat Completions that have been stored with the `store` parameter set to `true` will be returned.', [], none, [response(200, 'A list of Chat Completions', [media('application/json', @true)])]).
	operation(createChatCompletion, 'Chat', post, '/chat/completions', '**Starting a new project?** We recommend trying [Responses](/docs/api-reference/responses) to take advantage of the latest OpenAI platform features. Compare [Chat Completions with Responses](/docs/guides/responses-vs-chat-completions?api-mode=responses). --- Creates a model response for the given chat conversation. Learn more in the [text generation](/docs/guides/text-generation), [vision](/docs/guides/vision), and [audio](/docs/guides/audio) guides. Parameter support can differ depending on the model used to generate the response, particularly for newer reasoning models. Parameters that are only supported for reasoning models are noted below. For the current state of unsupported parameters in reasoning models, [refer to the reasoning guide](/docs/guides/reasoning). Returns a chat completion object, or a streamed sequence of chat completion chunk objects if the request is streamed.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteChatCompletion, 'Chat', delete, '/chat/completions/{completion_id}', 'Delete a stored chat completion. Only Chat Completions that have been created with the `store` parameter set to `true` can be deleted.', [parameter(completion_id, path, 'Path parameter.', true, @true)], none, [response(200, 'The chat completion was deleted successfully.', [media('application/json', @true)])]).
	operation(getChatCompletion, 'Chat', get, '/chat/completions/{completion_id}', 'Get a stored chat completion. Only Chat Completions that have been created with the `store` parameter set to `true` will be returned.', [parameter(completion_id, path, 'Path parameter.', true, @true)], none, [response(200, 'A chat completion', [media('application/json', @true)])]).
	operation(updateChatCompletion, 'Chat', post, '/chat/completions/{completion_id}', 'Modify a stored chat completion. Only Chat Completions that have been created with the `store` parameter set to `true` can be modified. Currently, the only supported modification is to update the `metadata` field.', [parameter(completion_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'A chat completion', [media('application/json', @true)])]).
	operation(getChatCompletionMessages, 'Chat', get, '/chat/completions/{completion_id}/messages', 'Get the messages in a stored chat completion. Only Chat Completions that have been created with the `store` parameter set to `true` will be returned.', [parameter(completion_id, path, 'Path parameter.', true, @true)], none, [response(200, 'A list of messages', [media('application/json', @true)])]).
	operation(createCompletion, 'Completions', post, '/completions', 'Creates a completion for the provided prompt and parameters. Returns a completion object, or a sequence of completion objects if the request is streamed.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(createConversation, 'Conversations', post, '/conversations', 'Create a conversation.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation(deleteConversation, 'Conversations', delete, '/conversations/{conversation_id}', 'Delete a conversation. Items in the conversation will not be deleted.', [parameter(conversation_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation(getConversation, 'Conversations', get, '/conversations/{conversation_id}', 'Get a conversation', [parameter(conversation_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation(updateConversation, 'Conversations', post, '/conversations/{conversation_id}', 'Update a conversation', [parameter(conversation_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation(listConversationItems, 'Conversations', get, '/conversations/{conversation_id}/items', 'List all items for a conversation with the given ID.', [parameter(conversation_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createConversationItems, 'Conversations', post, '/conversations/{conversation_id}/items', 'Create items in a conversation with the given ID.', [parameter(conversation_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteConversationItem, 'Conversations', delete, '/conversations/{conversation_id}/items/{item_id}', 'Delete an item from a conversation with the given IDs.', [parameter(conversation_id, path, 'Path parameter.', true, @true), parameter(item_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(getConversationItem, 'Conversations', get, '/conversations/{conversation_id}/items/{item_id}', 'Get a single item from a conversation with the given IDs.', [parameter(conversation_id, path, 'Path parameter.', true, @true), parameter(item_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createEmbedding, 'Embeddings', post, '/embeddings', 'Creates an embedding vector representing the input text.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(listEvals, 'Evals', get, '/evals', 'List evaluations for a project.', [], none, [response(200, 'A list of evals', [media('application/json', @true)])]).
	operation(createEval, 'Evals', post, '/evals', 'Create the structure of an evaluation that can be used to test a model''s performance. An evaluation is a set of testing criteria and the config for a data source, which dictates the schema of the data used in the evaluation. After creating an evaluation, you can run it on different models and model parameters. We support several types of graders and datasources. For more information, see the [Evals guide](/docs/guides/evals).', [], request_body('Request body.', false, [media('application/json', @true)]), [response(201, 'OK', [media('application/json', @true)])]).
	operation(deleteEval, 'Evals', delete, '/evals/{eval_id}', 'Delete an evaluation.', [parameter(eval_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Successfully deleted the evaluation.', [media('application/json', @true)]), response(404, 'Evaluation not found.', [media('application/json', @true)])]).
	operation(getEval, 'Evals', get, '/evals/{eval_id}', 'Get an evaluation by ID.', [parameter(eval_id, path, 'Path parameter.', true, @true)], none, [response(200, 'The evaluation', [media('application/json', @true)])]).
	operation(updateEval, 'Evals', post, '/evals/{eval_id}', 'Update certain properties of an evaluation.', [parameter(eval_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'The updated evaluation', [media('application/json', @true)])]).
	operation(getEvalRuns, 'Evals', get, '/evals/{eval_id}/runs', 'Get a list of runs for an evaluation.', [parameter(eval_id, path, 'Path parameter.', true, @true)], none, [response(200, 'A list of runs for the evaluation', [media('application/json', @true)])]).
	operation(createEvalRun, 'Evals', post, '/evals/{eval_id}/runs', 'Kicks off a new run for a given evaluation, specifying the data source, and what model configuration to use to test. The datasource will be validated against the schema specified in the config of the evaluation.', [parameter(eval_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(201, 'Successfully created a run for the evaluation', [media('application/json', @true)]), response(400, 'Bad request (for example, missing eval object)', [media('application/json', @true)])]).
	operation(deleteEvalRun, 'Evals', delete, '/evals/{eval_id}/runs/{run_id}', 'Delete an eval run.', [parameter(eval_id, path, 'Path parameter.', true, @true), parameter(run_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Successfully deleted the eval run', [media('application/json', @true)]), response(404, 'Run not found', [media('application/json', @true)])]).
	operation(getEvalRun, 'Evals', get, '/evals/{eval_id}/runs/{run_id}', 'Get an evaluation run by ID.', [parameter(eval_id, path, 'Path parameter.', true, @true), parameter(run_id, path, 'Path parameter.', true, @true)], none, [response(200, 'The evaluation run', [media('application/json', @true)])]).
	operation(cancelEvalRun, 'Evals', post, '/evals/{eval_id}/runs/{run_id}', 'Cancel an ongoing evaluation run.', [parameter(eval_id, path, 'Path parameter.', true, @true), parameter(run_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'The canceled eval run object', [media('application/json', @true)])]).
	operation(getEvalRunOutputItems, 'Evals', get, '/evals/{eval_id}/runs/{run_id}/output_items', 'Get a list of output items for an evaluation run.', [parameter(eval_id, path, 'Path parameter.', true, @true), parameter(run_id, path, 'Path parameter.', true, @true)], none, [response(200, 'A list of output items for the evaluation run', [media('application/json', @true)])]).
	operation(getEvalRunOutputItem, 'Evals', get, '/evals/{eval_id}/runs/{run_id}/output_items/{output_item_id}', 'Get an evaluation run output item by ID.', [parameter(eval_id, path, 'Path parameter.', true, @true), parameter(run_id, path, 'Path parameter.', true, @true), parameter(output_item_id, path, 'Path parameter.', true, @true)], none, [response(200, 'The evaluation run output item', [media('application/json', @true)])]).
	operation(listFiles, 'Files', get, '/files', 'Returns a list of files.', [], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createFile, 'Files', post, '/files', 'Upload a file that can be used across various endpoints. Individual files can be up to 512 MB, and each project can store up to 2.5 TB of files in total. There is no organization-wide storage limit. Uploads to this endpoint are rate-limited to 1,000 requests per minute per authenticated user. - The Assistants API supports files up to 2 million tokens and of specific file types. See the [Assistants Tools guide](/docs/assistants/tools) for details. - The Fine-tuning API only supports `.jsonl` files. The input also has certain required formats for fine-tuning [chat](/docs/api-reference/fine-tuning/chat-input) or [completions](/docs/api-reference/fine-tuning/completions-input) models. - The Batch API only supports `.jsonl` files up to 200 MB in size. The input also has a specific required [format](/docs/api-reference/batch/request-input). - For Retrieval or `file_search` ingestion, upload files here first. If you need to attach multiple uploaded files to the same vector store, use [`/vector_stores/{vector_store_id}/file_batches`](/docs/api-reference/vector-stores-file-batches/createBatch) instead of attaching them one by one. Vector store attachment has separate limits from file upload, including 2,000 attached files per minute per organization. Please [contact us](https://help.openai.com/) if you need to increase these storage limits.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteFile, 'Files', delete, '/files/{file_id}', 'Delete a file and remove it from all vector stores.', [parameter(file_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(retrieveFile, 'Files', get, '/files/{file_id}', 'Returns information about a specific file.', [parameter(file_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(downloadFile, 'Files', get, '/files/{file_id}/content', 'Returns the contents of the specified file.', [parameter(file_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(runGrader, 'Fine-tuning', post, '/fine_tuning/alpha/graders/run', 'Run a grader.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(validateGrader, 'Fine-tuning', post, '/fine_tuning/alpha/graders/validate', 'Validate a grader.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(listFineTuningCheckpointPermissions, 'Fine-tuning', get, '/fine_tuning/checkpoints/{fine_tuned_model_checkpoint}/permissions', '**NOTE:** This endpoint requires an [admin API key](../admin-api-keys). Organization owners can use this endpoint to view all permissions for a fine-tuned model checkpoint.', [parameter(fine_tuned_model_checkpoint, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createFineTuningCheckpointPermission, 'Fine-tuning', post, '/fine_tuning/checkpoints/{fine_tuned_model_checkpoint}/permissions', '**NOTE:** Calling this endpoint requires an [admin API key](../admin-api-keys). This enables organization owners to share fine-tuned models with other projects in their organization.', [parameter(fine_tuned_model_checkpoint, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteFineTuningCheckpointPermission, 'Fine-tuning', delete, '/fine_tuning/checkpoints/{fine_tuned_model_checkpoint}/permissions/{permission_id}', '**NOTE:** This endpoint requires an [admin API key](../admin-api-keys). Organization owners can use this endpoint to delete a permission for a fine-tuned model checkpoint.', [parameter(fine_tuned_model_checkpoint, path, 'Path parameter.', true, @true), parameter(permission_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(listPaginatedFineTuningJobs, 'Fine-tuning', get, '/fine_tuning/jobs', 'List your organization''s fine-tuning jobs', [], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createFineTuningJob, 'Fine-tuning', post, '/fine_tuning/jobs', 'Creates a fine-tuning job which begins the process of creating a new model from a given dataset. Response includes details of the enqueued job including job status and the name of the fine-tuned models once complete. [Learn more about fine-tuning](/docs/guides/model-optimization)', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(retrieveFineTuningJob, 'Fine-tuning', get, '/fine_tuning/jobs/{fine_tuning_job_id}', 'Get info about a fine-tuning job. [Learn more about fine-tuning](/docs/guides/model-optimization)', [parameter(fine_tuning_job_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(cancelFineTuningJob, 'Fine-tuning', post, '/fine_tuning/jobs/{fine_tuning_job_id}/cancel', 'Immediately cancel a fine-tune job.', [parameter(fine_tuning_job_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(listFineTuningJobCheckpoints, 'Fine-tuning', get, '/fine_tuning/jobs/{fine_tuning_job_id}/checkpoints', 'List checkpoints for a fine-tuning job.', [parameter(fine_tuning_job_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(listFineTuningEvents, 'Fine-tuning', get, '/fine_tuning/jobs/{fine_tuning_job_id}/events', 'Get status updates for a fine-tuning job.', [parameter(fine_tuning_job_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(pauseFineTuningJob, 'Fine-tuning', post, '/fine_tuning/jobs/{fine_tuning_job_id}/pause', 'Pause a fine-tune job.', [parameter(fine_tuning_job_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(resumeFineTuningJob, 'Fine-tuning', post, '/fine_tuning/jobs/{fine_tuning_job_id}/resume', 'Resume a fine-tune job.', [parameter(fine_tuning_job_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(createImageEdit, 'Images', post, '/images/edits', 'Creates an edited or extended image given one or more source images and a prompt. This endpoint supports GPT Image models (`gpt-image-1.5`, `gpt-image-1`, `gpt-image-1-mini`, and `chatgpt-image-latest`) and `dall-e-2`.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(createImage, 'Images', post, '/images/generations', 'Creates an image given a prompt. [Learn more](/docs/guides/images).', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(createImageVariation, 'Images', post, '/images/variations', 'Creates a variation of a given image. This endpoint only supports `dall-e-2`.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(listModels, 'Models', get, '/models', 'Lists the currently available models, and provides basic information about each one such as the owner and availability.', [], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteModel, 'Models', delete, '/models/{model}', 'Delete a fine-tuned model. You must have the Owner role in your organization to delete a model.', [parameter(model, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(retrieveModel, 'Models', get, '/models/{model}', 'Retrieves a model instance, providing basic information about the model such as the owner and permissioning.', [parameter(model, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createModeration, 'Moderations', post, '/moderations', 'Classifies if text and/or image inputs are potentially harmful. Learn more in the [moderation guide](/docs/guides/moderation).', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation('create-realtime-call', 'Realtime', post, '/realtime/calls', 'Create a new Realtime API call over WebRTC and receive the SDP answer needed to complete the peer connection.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(201, 'Realtime call created successfully.', [media('application/json', @true)])]).
	operation('accept-realtime-call', 'Realtime', post, '/realtime/calls/{call_id}/accept', 'Accept an incoming SIP call and configure the realtime session that will handle it.', [parameter(call_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Call accepted successfully.', [media('application/json', @true)])]).
	operation('hangup-realtime-call', 'Realtime', post, '/realtime/calls/{call_id}/hangup', 'End an active Realtime API call, whether it was initiated over SIP or WebRTC.', [parameter(call_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Call hangup initiated successfully.', [media('application/json', @true)])]).
	operation('refer-realtime-call', 'Realtime', post, '/realtime/calls/{call_id}/refer', 'Transfer an active SIP call to a new destination using the SIP REFER verb.', [parameter(call_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Call referred successfully.', [media('application/json', @true)])]).
	operation('reject-realtime-call', 'Realtime', post, '/realtime/calls/{call_id}/reject', 'Decline an incoming SIP call by returning a SIP status code to the caller.', [parameter(call_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Call rejected successfully.', [media('application/json', @true)])]).
	operation('create-realtime-client-secret', 'Realtime', post, '/realtime/client_secrets', 'Create a Realtime client secret with an associated session configuration. Client secrets are short-lived tokens that can be passed to a client app, such as a web frontend or mobile client, which grants access to the Realtime API without leaking your main API key. You can configure a custom TTL for each client secret. You can also attach session configuration options to the client secret, which will be applied to any sessions created using that client secret, but these can also be overridden by the client connection. [Learn more about authentication with client secrets over WebRTC](/docs/guides/realtime-webrtc). Returns the created client secret and the effective session object. The client secret is a string that looks like `ek_1234`.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Client secret created successfully.', [media('application/json', @true)])]).
	operation('create-realtime-session', 'Realtime', post, '/realtime/sessions', 'Create an ephemeral API token for use in client-side applications with the Realtime API. Can be configured with the same session parameters as the `session.update` client event. It responds with a session object, plus a `client_secret` key which contains a usable ephemeral API token that can be used to authenticate browser clients for the Realtime API. Returns the created Realtime session object, plus an ephemeral key.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Session created successfully.', [media('application/json', @true)])]).
	operation('create-realtime-transcription-session', 'Realtime', post, '/realtime/transcription_sessions', 'Create an ephemeral API token for use in client-side applications with the Realtime API specifically for realtime transcriptions. Can be configured with the same session parameters as the `transcription_session.update` client event. It responds with a session object, plus a `client_secret` key which contains a usable ephemeral API token that can be used to authenticate browser clients for the Realtime API. Returns the created Realtime transcription session object, plus an ephemeral key.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Session created successfully.', [media('application/json', @true)])]).
	operation('create-realtime-translation-client-secret', 'Realtime', post, '/realtime/translations/client_secrets', 'Create a Realtime translation client secret with an associated translation session configuration. Client secrets are short-lived tokens that can be passed to a client app, such as a web frontend or mobile client, which grants access to the Realtime Translation API without leaking your main API key. You can configure a custom TTL for each client secret. Returns the created client secret and the effective translation session object. The client secret is a string that looks like `ek_1234`.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Translation client secret created successfully.', [media('application/json', @true)])]).
	operation(createResponse, 'Responses', post, '/responses', 'Creates a model response. Provide [text](/docs/guides/text) or [image](/docs/guides/images) inputs to generate [text](/docs/guides/text) or [JSON](/docs/guides/structured-outputs) outputs. Have the model call your own [custom code](/docs/guides/function-calling) or use built-in [tools](/docs/guides/tools) like [web search](/docs/guides/tools-web-search) or [file search](/docs/guides/tools-file-search) to use your own data as input for the model''s response.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteResponse, 'Responses', delete, '/responses/{response_id}', 'Deletes a model response with the given ID.', [parameter(response_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)]), response(404, 'Not Found', [media('application/json', @true)])]).
	operation(getResponse, 'Responses', get, '/responses/{response_id}', 'Retrieves a model response with the given ID.', [parameter(response_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(cancelResponse, 'Responses', post, '/responses/{response_id}/cancel', 'Cancels a model response with the given ID. Only responses created with the `background` parameter set to `true` can be cancelled. [Learn more](/docs/guides/background).', [parameter(response_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)]), response(404, 'Not Found', [media('application/json', @true)])]).
	operation(listInputItems, 'Responses', get, '/responses/{response_id}/input_items', 'Returns a list of input items for a given response.', [parameter(response_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation('ListSkills', 'Skills', get, '/skills', 'List all skills for the current project.', [], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('CreateSkill', 'Skills', post, '/skills', 'Create a new skill.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('DeleteSkill', 'Skills', delete, '/skills/{skill_id}', 'Delete a skill by its ID.', [parameter(skill_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('GetSkill', 'Skills', get, '/skills/{skill_id}', 'Get a skill by its ID.', [parameter(skill_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('UpdateSkillDefaultVersion', 'Skills', post, '/skills/{skill_id}', 'Update the default version pointer for a skill.', [parameter(skill_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('GetSkillContent', 'Skills', get, '/skills/{skill_id}/content', 'Download a skill zip bundle by its ID.', [parameter(skill_id, path, 'Path parameter.', true, @true)], none, [response(200, 'The skill zip bundle.', [media('application/json', @true)])]).
	operation('ListSkillVersions', 'Skills', get, '/skills/{skill_id}/versions', 'List skill versions for a skill.', [parameter(skill_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('CreateSkillVersion', 'Skills', post, '/skills/{skill_id}/versions', 'Create a new immutable skill version.', [parameter(skill_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('DeleteSkillVersion', 'Skills', delete, '/skills/{skill_id}/versions/{version}', 'Delete a skill version.', [parameter(skill_id, path, 'Path parameter.', true, @true), parameter(version, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('GetSkillVersion', 'Skills', get, '/skills/{skill_id}/versions/{version}', 'Get a specific skill version.', [parameter(skill_id, path, 'Path parameter.', true, @true), parameter(version, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('GetSkillVersionContent', 'Skills', get, '/skills/{skill_id}/versions/{version}/content', 'Download a skill version zip bundle.', [parameter(skill_id, path, 'Path parameter.', true, @true), parameter(version, path, 'Path parameter.', true, @true)], none, [response(200, 'The skill zip bundle.', [media('application/json', @true)])]).
	operation('CreateChatSessionMethod', 'Unclassified', post, '/chatkit/sessions', 'Create a ChatKit session.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('CancelChatSessionMethod', 'Unclassified', post, '/chatkit/sessions/{session_id}/cancel', 'Cancel an active ChatKit session and return its most recent metadata. Cancelling prevents new requests from using the issued client secret.', [parameter(session_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('ListThreadsMethod', 'Unclassified', get, '/chatkit/threads', 'List ChatKit threads with optional pagination and user filters.', [], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('DeleteThreadMethod', 'Unclassified', delete, '/chatkit/threads/{thread_id}', 'Delete a ChatKit thread along with its items and stored attachments.', [parameter(thread_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('GetThreadMethod', 'Unclassified', get, '/chatkit/threads/{thread_id}', 'Retrieve a ChatKit thread by its identifier.', [parameter(thread_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('ListThreadItemsMethod', 'Unclassified', get, '/chatkit/threads/{thread_id}/items', 'List items that belong to a ChatKit thread.', [parameter(thread_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('ListContainers', 'Unclassified', get, '/containers', 'List Containers', [], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('CreateContainer', 'Unclassified', post, '/containers', 'Create Container', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('DeleteContainer', 'Unclassified', delete, '/containers/{container_id}', 'Delete Container', [parameter(container_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation('RetrieveContainer', 'Unclassified', get, '/containers/{container_id}', 'Retrieve Container', [parameter(container_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('ListContainerFiles', 'Unclassified', get, '/containers/{container_id}/files', 'List Container files', [parameter(container_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('CreateContainerFile', 'Unclassified', post, '/containers/{container_id}/files', 'Create a Container File You can send either a multipart/form-data request with the raw file content, or a JSON request with a file ID.', [parameter(container_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('DeleteContainerFile', 'Unclassified', delete, '/containers/{container_id}/files/{file_id}', 'Delete Container File', [parameter(container_id, path, 'Path parameter.', true, @true), parameter(file_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation('RetrieveContainerFile', 'Unclassified', get, '/containers/{container_id}/files/{file_id}', 'Retrieve Container File', [parameter(container_id, path, 'Path parameter.', true, @true), parameter(file_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('RetrieveContainerFileContent', 'Unclassified', get, '/containers/{container_id}/files/{file_id}/content', 'Retrieve Container File Content', [parameter(container_id, path, 'Path parameter.', true, @true), parameter(file_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('Compactconversation', 'Unclassified', post, '/responses/compact', 'Compact a conversation. Returns a compacted response object. Learn when and how to compact long-running conversations in the [conversation state guide](/docs/guides/conversation-state#managing-the-context-window). For ZDR-compatible compaction details, see [Compaction (advanced)](/docs/guides/conversation-state#compaction-advanced).', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('Getinputtokencounts', 'Unclassified', post, '/responses/input_tokens', 'Returns input token counts of the request. Returns an object with `object` set to `response.input_tokens` and an `input_tokens` count.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation(createUpload, 'Uploads', post, '/uploads', 'Creates an intermediate [Upload](/docs/api-reference/uploads/object) object that you can add [Parts](/docs/api-reference/uploads/part-object) to. Currently, an Upload can accept at most 8 GB in total and expires after an hour after you create it. Once you complete the Upload, we will create a [File](/docs/api-reference/files/object) object that contains all the parts you uploaded. This File is usable in the rest of our platform as a regular File object. For certain `purpose` values, the correct `mime_type` must be specified. Please refer to documentation for the [supported MIME types for your use case](/docs/assistants/tools/file-search#supported-files). For guidance on the proper filename extensions for each purpose, please follow the documentation on [creating a File](/docs/api-reference/files/create). Returns the Upload object with status `pending`.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(cancelUpload, 'Uploads', post, '/uploads/{upload_id}/cancel', 'Cancels the Upload. No Parts may be added after an Upload is cancelled. Returns the Upload object with status `cancelled`.', [parameter(upload_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(completeUpload, 'Uploads', post, '/uploads/{upload_id}/complete', 'Completes the [Upload](/docs/api-reference/uploads/object). Within the returned Upload object, there is a nested [File](/docs/api-reference/files/object) object that is ready to use in the rest of the platform. You can specify the order of the Parts by passing in an ordered list of the Part IDs. The number of bytes uploaded upon completion must match the number of bytes initially specified when creating the Upload object. No Parts may be added after an Upload is completed. Returns the Upload object with status `completed`, including an additional `file` property containing the created usable File object.', [parameter(upload_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(addUploadPart, 'Uploads', post, '/uploads/{upload_id}/parts', 'Adds a [Part](/docs/api-reference/uploads/part-object) to an [Upload](/docs/api-reference/uploads/object) object. A Part represents a chunk of bytes from the file you are trying to upload. Each Part can be at most 64 MB, and you can add Parts until you hit the Upload maximum of 8 GB. It is possible to add multiple Parts in parallel. You can decide the intended order of the Parts when you [complete the Upload](/docs/api-reference/uploads/complete).', [parameter(upload_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(listVectorStores, 'Vector stores', get, '/vector_stores', 'Returns a list of vector stores.', [], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createVectorStore, 'Vector stores', post, '/vector_stores', 'Create a vector store.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteVectorStore, 'Vector stores', delete, '/vector_stores/{vector_store_id}', 'Delete a vector store.', [parameter(vector_store_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(getVectorStore, 'Vector stores', get, '/vector_stores/{vector_store_id}', 'Retrieves a vector store.', [parameter(vector_store_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(modifyVectorStore, 'Vector stores', post, '/vector_stores/{vector_store_id}', 'Modifies a vector store.', [parameter(vector_store_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(createVectorStoreFileBatch, 'Vector stores', post, '/vector_stores/{vector_store_id}/file_batches', 'Create a vector store file batch.', [parameter(vector_store_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(getVectorStoreFileBatch, 'Vector stores', get, '/vector_stores/{vector_store_id}/file_batches/{batch_id}', 'Retrieves a vector store file batch.', [parameter(vector_store_id, path, 'Path parameter.', true, @true), parameter(batch_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(cancelVectorStoreFileBatch, 'Vector stores', post, '/vector_stores/{vector_store_id}/file_batches/{batch_id}/cancel', 'Cancel a vector store file batch. This attempts to cancel the processing of files in this batch as soon as possible.', [parameter(vector_store_id, path, 'Path parameter.', true, @true), parameter(batch_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(listFilesInVectorStoreBatch, 'Vector stores', get, '/vector_stores/{vector_store_id}/file_batches/{batch_id}/files', 'Returns a list of vector store files in a batch.', [parameter(vector_store_id, path, 'Path parameter.', true, @true), parameter(batch_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(listVectorStoreFiles, 'Vector stores', get, '/vector_stores/{vector_store_id}/files', 'Returns a list of vector store files.', [parameter(vector_store_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(createVectorStoreFile, 'Vector stores', post, '/vector_stores/{vector_store_id}/files', 'Create a vector store file by attaching a [File](/docs/api-reference/files) to a [vector store](/docs/api-reference/vector-stores/object).', [parameter(vector_store_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(deleteVectorStoreFile, 'Vector stores', delete, '/vector_stores/{vector_store_id}/files/{file_id}', 'Delete a vector store file. This will remove the file from the vector store but the file itself will not be deleted. To delete the file, use the [delete file](/docs/api-reference/files/delete) endpoint.', [parameter(vector_store_id, path, 'Path parameter.', true, @true), parameter(file_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(getVectorStoreFile, 'Vector stores', get, '/vector_stores/{vector_store_id}/files/{file_id}', 'Retrieves a vector store file.', [parameter(vector_store_id, path, 'Path parameter.', true, @true), parameter(file_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(updateVectorStoreFileAttributes, 'Vector stores', post, '/vector_stores/{vector_store_id}/files/{file_id}', 'Update attributes on a vector store file.', [parameter(vector_store_id, path, 'Path parameter.', true, @true), parameter(file_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation(retrieveVectorStoreFileContent, 'Vector stores', get, '/vector_stores/{vector_store_id}/files/{file_id}/content', 'Retrieve the parsed contents of a vector store file.', [parameter(vector_store_id, path, 'Path parameter.', true, @true), parameter(file_id, path, 'Path parameter.', true, @true)], none, [response(200, 'OK', [media('application/json', @true)])]).
	operation(searchVectorStore, 'Vector stores', post, '/vector_stores/{vector_store_id}/search', 'Search a vector store for relevant chunks based on a query and file attributes filter.', [parameter(vector_store_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'OK', [media('application/json', @true)])]).
	operation('ListVideos', 'Videos', get, '/videos', 'List recently generated videos for the current project.', [], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation(createVideo, 'Videos', post, '/videos', 'Create a new video generation job from a prompt and optional reference assets.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('CreateVideoCharacter', 'Videos', post, '/videos/characters', 'Create a character from an uploaded video.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('GetVideoCharacter', 'Videos', get, '/videos/characters/{character_id}', 'Fetch a character.', [parameter(character_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('CreateVideoEdit', 'Videos', post, '/videos/edits', 'Create a new video generation job by editing a source video or existing generated video.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('CreateVideoExtend', 'Videos', post, '/videos/extensions', 'Create an extension of a completed video.', [], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).
	operation('DeleteVideo', 'Videos', delete, '/videos/{video_id}', 'Permanently delete a completed or failed video and its stored assets.', [parameter(video_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('GetVideo', 'Videos', get, '/videos/{video_id}', 'Fetch the latest metadata for a generated video.', [parameter(video_id, path, 'Path parameter.', true, @true)], none, [response(200, 'Success', [media('application/json', @true)])]).
	operation('RetrieveVideoContent', 'Videos', get, '/videos/{video_id}/content', 'Download the generated video bytes or a derived preview asset. Streams the rendered video content for the specified video job.', [parameter(video_id, path, 'Path parameter.', true, @true)], none, [response(200, 'The video bytes or preview asset that matches the requested variant.', [media('application/json', @true)])]).
	operation('CreateVideoRemix', 'Videos', post, '/videos/{video_id}/remix', 'Create a remix of a completed video using a refreshed prompt.', [parameter(video_id, path, 'Path parameter.', true, @true)], request_body('Request body.', false, [media('application/json', @true)]), [response(200, 'Success', [media('application/json', @true)])]).

	operation_properties(listAssistants, [tags(['Assistants']), deprecated(true)]).
	operation_properties(createAssistant, [tags(['Assistants']), deprecated(true)]).
	operation_properties(deleteAssistant, [tags(['Assistants']), deprecated(true)]).
	operation_properties(getAssistant, [tags(['Assistants']), deprecated(true)]).
	operation_properties(modifyAssistant, [tags(['Assistants']), deprecated(true)]).
	operation_properties(createThread, [tags(['Assistants']), deprecated(false)]).
	operation_properties(createThreadAndRun, [tags(['Assistants']), deprecated(false)]).
	operation_properties(deleteThread, [tags(['Assistants']), deprecated(false)]).
	operation_properties(getThread, [tags(['Assistants']), deprecated(false)]).
	operation_properties(modifyThread, [tags(['Assistants']), deprecated(false)]).
	operation_properties(listMessages, [tags(['Assistants']), deprecated(false)]).
	operation_properties(createMessage, [tags(['Assistants']), deprecated(false)]).
	operation_properties(deleteMessage, [tags(['Assistants']), deprecated(false)]).
	operation_properties(getMessage, [tags(['Assistants']), deprecated(false)]).
	operation_properties(modifyMessage, [tags(['Assistants']), deprecated(false)]).
	operation_properties(listRuns, [tags(['Assistants']), deprecated(false)]).
	operation_properties(createRun, [tags(['Assistants']), deprecated(false)]).
	operation_properties(getRun, [tags(['Assistants']), deprecated(false)]).
	operation_properties(modifyRun, [tags(['Assistants']), deprecated(false)]).
	operation_properties(cancelRun, [tags(['Assistants']), deprecated(false)]).
	operation_properties(listRunSteps, [tags(['Assistants']), deprecated(false)]).
	operation_properties(getRunStep, [tags(['Assistants']), deprecated(false)]).
	operation_properties(submitToolOuputsToRun, [tags(['Assistants']), deprecated(false)]).
	operation_properties(createSpeech, [tags(['Audio']), deprecated(false)]).
	operation_properties(createTranscription, [tags(['Audio']), deprecated(false)]).
	operation_properties(createTranslation, [tags(['Audio']), deprecated(false)]).
	operation_properties(listVoiceConsents, [description('List consent recordings available to your organization for creating custom voices. See the [custom voices guide](/docs/guides/text-to-speech#custom-voices). Custom voices are limited to eligible customers.'), tags(['Audio']), deprecated(false)]).
	operation_properties(createVoiceConsent, [description('Upload a consent recording that authorizes creation of a custom voice. See the [custom voices guide](/docs/guides/text-to-speech#custom-voices) for requirements and best practices. Custom voices are limited to eligible customers.'), tags(['Audio']), deprecated(false)]).
	operation_properties(deleteVoiceConsent, [description('Delete a consent recording that was uploaded for creating custom voices. See the [custom voices guide](/docs/guides/text-to-speech#custom-voices). Custom voices are limited to eligible customers.'), tags(['Audio']), deprecated(false)]).
	operation_properties(getVoiceConsent, [description('Retrieve consent recording metadata used for creating custom voices. See the [custom voices guide](/docs/guides/text-to-speech#custom-voices). Custom voices are limited to eligible customers.'), tags(['Audio']), deprecated(false)]).
	operation_properties(updateVoiceConsent, [description('Update consent recording metadata used for creating custom voices. This endpoint updates metadata only and does not replace the underlying audio. See the [custom voices guide](/docs/guides/text-to-speech#custom-voices). Custom voices are limited to eligible customers.'), tags(['Audio']), deprecated(false)]).
	operation_properties(createVoice, [description('Create a custom voice you can use for audio output (for example, in Text-to-Speech and the Realtime API). This requires an audio sample and a previously uploaded consent recording. See the [custom voices guide](/docs/guides/text-to-speech#custom-voices) for requirements and best practices. Custom voices are limited to eligible customers.'), tags(['Audio']), deprecated(false)]).
	operation_properties(listBatches, [tags(['Batch']), deprecated(false)]).
	operation_properties(createBatch, [tags(['Batch']), deprecated(false)]).
	operation_properties(retrieveBatch, [tags(['Batch']), deprecated(false)]).
	operation_properties(cancelBatch, [tags(['Batch']), deprecated(false)]).
	operation_properties(listChatCompletions, [tags(['Chat']), deprecated(false)]).
	operation_properties(createChatCompletion, [tags(['Chat']), deprecated(false)]).
	operation_properties(deleteChatCompletion, [tags(['Chat']), deprecated(false)]).
	operation_properties(getChatCompletion, [tags(['Chat']), deprecated(false)]).
	operation_properties(updateChatCompletion, [tags(['Chat']), deprecated(false)]).
	operation_properties(getChatCompletionMessages, [tags(['Chat']), deprecated(false)]).
	operation_properties(createCompletion, [tags(['Completions']), deprecated(false)]).
	operation_properties(createConversation, [tags(['Conversations']), deprecated(false)]).
	operation_properties(deleteConversation, [tags(['Conversations']), deprecated(false)]).
	operation_properties(getConversation, [tags(['Conversations']), deprecated(false)]).
	operation_properties(updateConversation, [tags(['Conversations']), deprecated(false)]).
	operation_properties(listConversationItems, [tags(['Conversations']), deprecated(false)]).
	operation_properties(createConversationItems, [tags(['Conversations']), deprecated(false)]).
	operation_properties(deleteConversationItem, [tags(['Conversations']), deprecated(false)]).
	operation_properties(getConversationItem, [tags(['Conversations']), deprecated(false)]).
	operation_properties(createEmbedding, [tags(['Embeddings']), deprecated(false)]).
	operation_properties(listEvals, [tags(['Evals']), deprecated(false)]).
	operation_properties(createEval, [tags(['Evals']), deprecated(false)]).
	operation_properties(deleteEval, [tags(['Evals']), deprecated(false)]).
	operation_properties(getEval, [tags(['Evals']), deprecated(false)]).
	operation_properties(updateEval, [tags(['Evals']), deprecated(false)]).
	operation_properties(getEvalRuns, [tags(['Evals']), deprecated(false)]).
	operation_properties(createEvalRun, [tags(['Evals']), deprecated(false)]).
	operation_properties(deleteEvalRun, [tags(['Evals']), deprecated(false)]).
	operation_properties(getEvalRun, [tags(['Evals']), deprecated(false)]).
	operation_properties(cancelEvalRun, [tags(['Evals']), deprecated(false)]).
	operation_properties(getEvalRunOutputItems, [tags(['Evals']), deprecated(false)]).
	operation_properties(getEvalRunOutputItem, [tags(['Evals']), deprecated(false)]).
	operation_properties(listFiles, [tags(['Files']), deprecated(false)]).
	operation_properties(createFile, [tags(['Files']), deprecated(false)]).
	operation_properties(deleteFile, [tags(['Files']), deprecated(false)]).
	operation_properties(retrieveFile, [tags(['Files']), deprecated(false)]).
	operation_properties(downloadFile, [tags(['Files']), deprecated(false)]).
	operation_properties(runGrader, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(validateGrader, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(listFineTuningCheckpointPermissions, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(createFineTuningCheckpointPermission, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(deleteFineTuningCheckpointPermission, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(listPaginatedFineTuningJobs, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(createFineTuningJob, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(retrieveFineTuningJob, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(cancelFineTuningJob, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(listFineTuningJobCheckpoints, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(listFineTuningEvents, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(pauseFineTuningJob, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(resumeFineTuningJob, [tags(['Fine-tuning']), deprecated(false)]).
	operation_properties(createImageEdit, [description('You can call this endpoint with either: - `multipart/form-data`: use binary uploads via `image` (and optional `mask`). - `application/json`: use `images` (and optional `mask`) as references with either `image_url` or `file_id`. Note that JSON requests use `images` (array) instead of the multipart `image` field.'), tags(['Images']), deprecated(false)]).
	operation_properties(createImage, [tags(['Images']), deprecated(false)]).
	operation_properties(createImageVariation, [tags(['Images']), deprecated(false)]).
	operation_properties(listModels, [tags(['Models']), deprecated(false)]).
	operation_properties(deleteModel, [tags(['Models']), deprecated(false)]).
	operation_properties(retrieveModel, [tags(['Models']), deprecated(false)]).
	operation_properties(createModeration, [tags(['Moderations']), deprecated(false)]).
	operation_properties('create-realtime-call', [tags(['Realtime']), deprecated(false)]).
	operation_properties('accept-realtime-call', [tags(['Realtime']), deprecated(false)]).
	operation_properties('hangup-realtime-call', [tags(['Realtime']), deprecated(false)]).
	operation_properties('refer-realtime-call', [tags(['Realtime']), deprecated(false)]).
	operation_properties('reject-realtime-call', [tags(['Realtime']), deprecated(false)]).
	operation_properties('create-realtime-client-secret', [tags(['Realtime']), deprecated(false)]).
	operation_properties('create-realtime-session', [tags(['Realtime']), deprecated(false)]).
	operation_properties('create-realtime-transcription-session', [tags(['Realtime']), deprecated(false)]).
	operation_properties('create-realtime-translation-client-secret', [tags(['Realtime']), deprecated(false)]).
	operation_properties(createResponse, [tags(['Responses']), deprecated(false)]).
	operation_properties(deleteResponse, [tags(['Responses']), deprecated(false)]).
	operation_properties(getResponse, [tags(['Responses']), deprecated(false)]).
	operation_properties(cancelResponse, [tags(['Responses']), deprecated(false)]).
	operation_properties(listInputItems, [tags(['Responses']), deprecated(false)]).
	operation_properties('ListSkills', [tags(['Skills']), deprecated(false)]).
	operation_properties('CreateSkill', [tags(['Skills']), deprecated(false)]).
	operation_properties('DeleteSkill', [tags(['Skills']), deprecated(false)]).
	operation_properties('GetSkill', [tags(['Skills']), deprecated(false)]).
	operation_properties('UpdateSkillDefaultVersion', [tags(['Skills']), deprecated(false)]).
	operation_properties('GetSkillContent', [tags(['Skills']), deprecated(false)]).
	operation_properties('ListSkillVersions', [tags(['Skills']), deprecated(false)]).
	operation_properties('CreateSkillVersion', [tags(['Skills']), deprecated(false)]).
	operation_properties('DeleteSkillVersion', [tags(['Skills']), deprecated(false)]).
	operation_properties('GetSkillVersion', [tags(['Skills']), deprecated(false)]).
	operation_properties('GetSkillVersionContent', [tags(['Skills']), deprecated(false)]).
	operation_properties('CreateChatSessionMethod', [tags(['Unclassified']), deprecated(false)]).
	operation_properties('CancelChatSessionMethod', [tags(['Unclassified']), deprecated(false)]).
	operation_properties('ListThreadsMethod', [tags(['Unclassified']), deprecated(false)]).
	operation_properties('DeleteThreadMethod', [tags(['Unclassified']), deprecated(false)]).
	operation_properties('GetThreadMethod', [tags(['Unclassified']), deprecated(false)]).
	operation_properties('ListThreadItemsMethod', [tags(['Unclassified']), deprecated(false)]).
	operation_properties('ListContainers', [description('Lists containers.'), tags(['Unclassified']), deprecated(false)]).
	operation_properties('CreateContainer', [description('Creates a container.'), tags(['Unclassified']), deprecated(false)]).
	operation_properties('DeleteContainer', [description('Delete a container.'), tags(['Unclassified']), deprecated(false)]).
	operation_properties('RetrieveContainer', [description('Retrieves a container.'), tags(['Unclassified']), deprecated(false)]).
	operation_properties('ListContainerFiles', [description('Lists container files.'), tags(['Unclassified']), deprecated(false)]).
	operation_properties('CreateContainerFile', [description('Creates a container file.'), tags(['Unclassified']), deprecated(false)]).
	operation_properties('DeleteContainerFile', [description('Delete a container file.'), tags(['Unclassified']), deprecated(false)]).
	operation_properties('RetrieveContainerFile', [description('Retrieves a container file.'), tags(['Unclassified']), deprecated(false)]).
	operation_properties('RetrieveContainerFileContent', [description('Retrieves a container file content.'), tags(['Unclassified']), deprecated(false)]).
	operation_properties('Compactconversation', [tags(['Unclassified']), deprecated(false)]).
	operation_properties('Getinputtokencounts', [tags(['Unclassified']), deprecated(false)]).
	operation_properties(createUpload, [tags(['Uploads']), deprecated(false)]).
	operation_properties(cancelUpload, [tags(['Uploads']), deprecated(false)]).
	operation_properties(completeUpload, [tags(['Uploads']), deprecated(false)]).
	operation_properties(addUploadPart, [tags(['Uploads']), deprecated(false)]).
	operation_properties(listVectorStores, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(createVectorStore, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(deleteVectorStore, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(getVectorStore, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(modifyVectorStore, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(createVectorStoreFileBatch, [description('The maximum number of files in a single batch request is 2000. Vector store file attach requests are rate limited per vector store (300 requests per minute across both this endpoint and `/vector_stores/{vector_store_id}/files`). For ingesting multiple files into the same vector store, this batch endpoint is recommended.'), tags(['Vector stores']), deprecated(false)]).
	operation_properties(getVectorStoreFileBatch, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(cancelVectorStoreFileBatch, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(listFilesInVectorStoreBatch, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(listVectorStoreFiles, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(createVectorStoreFile, [description('This endpoint is subject to a per-vector-store write rate limit of 300 requests per minute, shared with `/vector_stores/{vector_store_id}/file_batches`. For uploading multiple files to the same vector store, use the file batches endpoint to reduce request volume.'), tags(['Vector stores']), deprecated(false)]).
	operation_properties(deleteVectorStoreFile, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(getVectorStoreFile, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(updateVectorStoreFileAttributes, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(retrieveVectorStoreFileContent, [tags(['Vector stores']), deprecated(false)]).
	operation_properties(searchVectorStore, [tags(['Vector stores']), deprecated(false)]).
	operation_properties('ListVideos', [tags(['Videos']), deprecated(false)]).
	operation_properties(createVideo, [tags(['Videos']), deprecated(false)]).
	operation_properties('CreateVideoCharacter', [tags(['Videos']), deprecated(false)]).
	operation_properties('GetVideoCharacter', [tags(['Videos']), deprecated(false)]).
	operation_properties('CreateVideoEdit', [tags(['Videos']), deprecated(false)]).
	operation_properties('CreateVideoExtend', [tags(['Videos']), deprecated(false)]).
	operation_properties('DeleteVideo', [tags(['Videos']), deprecated(false)]).
	operation_properties('GetVideo', [tags(['Videos']), deprecated(false)]).
	operation_properties('RetrieveVideoContent', [tags(['Videos']), deprecated(false)]).
	operation_properties('CreateVideoRemix', [tags(['Videos']), deprecated(false)]).

:- end_object.
