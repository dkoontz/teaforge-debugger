  # Time Travel Debugger JSONL Format Design

  ## Overview

  A JSONL-based format for capturing TEA (The Elm Architecture) application state, enabling time travel debugging across any
  compatible application (TeaForge, Elm, etc.).

  ## Format Structure

  Each line is a self-contained JSON object with an `entryType` discriminator:
  - `header` - Type registry and metadata (first line)
  - `init` - Initial state and effects from `init()`
  - `update` - Message processing result (message + state + effects)
  - `subscriptionChange` - Subscription lifecycle events

  ---

  ## Entry Types

  ### 1. Header Entry

  First line of the file. Contains type registry for all ADTs.

  ```json
  {
  "entryType": "header",
  "version": "1.0.0",
  "appId": "frc.robot.TdsRobot",
  "timestamp": 1706000000000,
  "typeRegistry": {
  "messages": {
  "Message": {
  "kind": "sealed_interface",
  "variants": [
  {"name": "Input", "qualifiedName": "Message.Input", "fields": [{"name": "message", "type":
  "InputSubsystem.Message"}]},
  {"name": "Swerve", "qualifiedName": "Message.Swerve", "fields": [{"name": "message", "type":
  "SwerveSubsystem.Message"}]}
  ]
  },
  "SwerveSubsystem.Message": {
  "kind": "sealed_interface",
  "variants": [
  {"name": "UpdateSwervePodPosition", "qualifiedName": "SwerveSubsystem.Message.UpdateSwervePodPosition", "fields":
  [{"name": "pod", "type": "SwervePod"}, {"name": "position", "type": "Double"}]},
  {"name": "UpdateHeading", "qualifiedName": "SwerveSubsystem.Message.UpdateHeading", "fields": [{"name": "value",
  "type": "Rotation3d"}]}
  ]
  }
  },
  "effects": { /* similar structure */ },
  "subscriptions": { /* similar structure */ },
  "dataObjects": {
  "SwervePod": {
  "kind": "sealed_interface",
  "variants": [
  {"name": "FrontLeft", "qualifiedName": "SwervePod.FrontLeft", "kind": "data_object"}
  ]
  }
  }
  }
  }
  ```

  ### 2. Init Entry

  Captures `init()` result - initial model and bootstrap effects.

  ```json
  {
  "entryType": "init",
  "sequence": 0,
  "timestamp": 1706000000100,
  "model": {
  "_type": "Model",
  "swerveModel": {
  "_type": "SwerveSubsystem.Model",
  "heading": 0.0,
  "steeringMotorPositions": {},
  "pigeonToken": {"_type": "Maybe.None"}
  }
  },
  "effects": [
  {"_type": "Effect.InitCanDevice", "type": "Talon", "id": 4, "callback": {"_callbackId": "cb_001", "_producesMessageType":
  "InitializationSubsystem.Message.InitCanDevice.InitKraken"}}
  ]
  }
  ```

  ### 3. Update Entry

  Captures each message dispatch through `update()`.

  ```json
  {
  "entryType": "update",
  "sequence": 15,
  "timestamp": 1706000001500,
  "message": {
  "_type": "Message.Swerve",
  "_unwrapped": {
  "_type": "SwerveSubsystem.Message.UpdateSwervePodPosition",
  "pod": {"_type": "SwervePod.FrontLeft"},
  "position": 45.7
  }
  },
  "model": {
  "_type": "Model",
  "swerveModel": {
  "steeringMotorPositions": {"SwervePod.FrontLeft": 45.7}
  }
  },
  "effects": []
  }
  ```

  ### 4. Subscription Change Entry

  Captures when subscriptions start or stop.

  ```json
  {
  "entryType": "subscriptionChange",
  "sequence": 43,
  "timestamp": 1706000005100,
  "started": [
  {
  "_subscriptionId": "sub_010",
  "_type": "Subscription.CANcoderValue",
  "token": {"_type": "EncoderToken", "_tokenId": "encoder_can_9"},
  "millisecondsBetweenReads": 10,
  "callback": {"_producesMessageType": "SwerveSubsystem.Message.UpdateSwervePodPosition"},
  "_reason": "Token became available"
  }
  ],
  "stopped": []
  }
  ```

  ---

  ## Serialization Conventions

  ### Type Identification
  All typed values include `_type` with qualified path:
  ```json
  {"_type": "SwerveSubsystem.Message.UpdateSwervePodPosition", "pod": {...}, "position": 45.7}
  ```

  ### Wrapper Messages
  Include `_unwrapped` to show inner message:
  ```json
  {"_type": "Message.Swerve", "_unwrapped": {"_type": "SwerveSubsystem.Message.UpdateHeading", "value": {...}}}
  ```

  ### Data Objects (zero-argument variants)
  ```json
  {"_type": "SwervePod.FrontLeft"}
  {"_type": "Maybe.None"}
  ```

  ### Opaque Tokens
  ```json
  {"_type": "KrakenMotorToken", "_tokenId": "kraken_can_4", "_opaqueHandle": true}
  ```

  ### Callbacks
  ```json
  {"_callbackId": "cb_001", "_signature": "(Double) -> Message", "_producesMessageType":
  "SwerveSubsystem.Message.UpdateSwervePodPosition"}
  ```

  ### Result/Maybe Types
  ```json
  {"_type": "Result.Success", "value": {...}}
  {"_type": "Result.Error", "error": {"_type": "Error.DeviceNotFound", "message": "..."}}
  {"_type": "Maybe.Some", "value": {...}}
  {"_type": "Maybe.None"}
  ```

  ### Maps with ADT Keys
  ```json
  {"steeringMotorPositions": {"SwervePod.FrontLeft": 45.7, "SwervePod.BackRight": 90.2}}
  ```

  ---

  ## Design Decisions

  1. **Model in updates**: New model only. Debugger reconstructs previous state from prior entries.
  2. **Subscription changes**: Separate entry type with own sequence number and timestamp.
  3. **Header generation**: Runtime reflection (one-time ~10-50ms startup cost). Can be done lazily or in background thread if
  needed.

  ---

  ## Example Stream

  ```
  {"entryType":"header","version":"1.0.0","appId":"frc.robot.TdsRobot",...}
  {"entryType":"init","sequence":0,"model":{...},"effects":[...]}
  {"entryType":"update","sequence":1,"message":{...},"model":{...},"effects":[...]}
  {"entryType":"subscriptionChange","sequence":2,"started":[...],"stopped":[]}
  {"entryType":"update","sequence":3,"message":{...},"model":{...},"effects":[]}
  ```