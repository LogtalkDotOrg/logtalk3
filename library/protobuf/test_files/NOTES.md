# Protocol Buffers Test Files

This directory contains sample Protocol Buffers schema files used for testing the Logtalk `protobuf` library.

## File Sources and Licenses

### person.proto and person.json

- **Description**: Simple Person message schema demonstrating basic field types
- **Source**: Adapted from the official Protocol Buffers tutorial
- **Original URL**: https://protobuf.dev/getting-started/
- **License**: Apache License 2.0 (compatible with Protocol Buffers documentation)
- **Modifications**: Simplified for testing purposes; JSON representation added for Logtalk compatibility

### addressbook.proto and addressbook.json

- **Description**: AddressBook schema demonstrating nested messages
- **Source**: Adapted from the official Protocol Buffers tutorial
- **Original URL**: https://protobuf.dev/getting-started/
- **License**: Apache License 2.0
- **Modifications**: Simplified for testing purposes; JSON representation added for Logtalk compatibility

## File Formats

- `.proto` files are reference files showing the standard Protocol Buffers schema syntax
- `.json` files contain the schema representation used by the Logtalk library
- The JSON schema format uses a structure compatible with the `json(curly, dash, atom)` library

## Usage

These schema files are automatically loaded by the test suite in `tests.lgt`. The tests demonstrate:

- Encoding and decoding of Protocol Buffers binary data
- Round-trip serialization (encode → decode → verify)
- Compatibility with the Protocol Buffers wire format specification
