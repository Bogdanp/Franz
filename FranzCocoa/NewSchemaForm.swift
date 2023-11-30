import NoiseSerde
import SwiftUI

struct NewSchemaForm: View {
  @State private var name = ""
  @State private var type = SchemaType.avro
  @State private var prevType = SchemaType.avro
  @State private var code = getExample(forType: .avro)

  let createAction: (String, SchemaType, String) -> Void
  let cancelAction: () -> Void

  var body: some View {
    Form {
      TextField(text: $name) {
        Text("Name:")
      }
      Picker("Schema Type:", selection: $type) {
        Text("AVRO").tag(SchemaType.avro)
        Text("JSON").tag(SchemaType.json)
        Text("Protocol Buffers").tag(SchemaType.protobuf)
      }.onChange(of: type) { _ in
        let example = getExample(forType: prevType)
        if code == example {
          code = getExample(forType: type)
        }
        prevType = type
      }
      Editor(
        code: $code,
        language: type == .protobuf ? .protobuf : .json,
        border: .bezelBorder,
        isEditable: true
      )
      HStack {
        Button("Create Schema") {
          createAction(name, type, code)
        }
        .keyboardShortcut(.defaultAction)
        .buttonStyle(.bordered)
        .disabled(name == "")
        Button("Cancel", role: .cancel) {
          cancelAction()
        }
        .keyboardShortcut(.cancelAction)
      }
    }
  }
}

fileprivate func getExample(forType type: SchemaType) -> String {
  switch type {
  case .avro:
    return """
{
  "type": "record",
  "name": "Record",
  "namespace": "com.example",
  "fields" : [
    {"name": "id", "type": "long"}
  ]
}
"""
  case .json:
    return """
{
  "$id": "https://example.com/record",
  "$schema": "https://json-schema.org/draft/2019-09/schema",
  "type": "object",
  "title": "Record",
  "description": "Record JSON Schema",
  "properties": {
    "id": {
      "type": "string"
    }
  },
  "required": [ "id" ],
  "additionalProperties": false
}
"""
  case .protobuf:
    return """
syntax = "proto3";

message Record {
  int32 id = 1;
}
"""
  }
}
