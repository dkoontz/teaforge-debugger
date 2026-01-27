# The Debugging Context

 When debugging a TEA application with thousands of messages, the core problem is signal vs noise. A user typically has a hypothesis ("the
 elevator position isn't updating correctly") and needs to find the relevant messages among a sea of irrelevant ones.

 Filtering Dimensions

 1. By Message Identity (Name Patterns)

 Use case: "I only care about ElevatorSubsystem messages right now"
 ┌─────────────────────────────┬──────────────────────────────────────────┬──────────────────────────────────────┐
 │         Filter Type         │                 Example                  │             When Useful              │
 ├─────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────┤
 │ Include by exact name       │ ElevatorSubsystem.Message.UpdatePosition │ Debugging a specific message handler │
 ├─────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────┤
 │ Include by prefix/hierarchy │ ElevatorSubsystem.*                      │ Debugging a subsystem                │
 ├─────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────┤
 │ Exclude by name             │ Hide UpdateTime                          │ Filtering out high-frequency noise   │
 ├─────────────────────────────┼──────────────────────────────────────────┼──────────────────────────────────────┤
 │ Exclude by prefix           │ Hide InputSubsystem.*                    │ Ignoring an irrelevant subsystem     │
 └─────────────────────────────┴──────────────────────────────────────────┴──────────────────────────────────────┘
 Interaction patterns:
 - Filter bar with text input: Type pattern, choose include/exclude
 - Context menu on message: Right-click → "Show only this type" / "Hide this type" / "Hide all from this subsystem"
 - Keyboard shortcut: Select message, press H to hide its type, O to show only its type

 2. By State Changes

 Use case: "Show me only the messages that actually did something"
 ┌──────────────────────────────────────┬───────────────────────────────────┬────────────────────────────────┐
 │             Filter Type              │              Example              │          When Useful           │
 ├──────────────────────────────────────┼───────────────────────────────────┼────────────────────────────────┤
 │ Only messages with state changes     │ Hide no-ops                       │ Focusing on impactful messages │
 ├──────────────────────────────────────┼───────────────────────────────────┼────────────────────────────────┤
 │ Messages that change a field         │ Changes to model.elevatorPosition │ Watching a specific value      │
 ├──────────────────────────────────────┼───────────────────────────────────┼────────────────────────────────┤
 │ Messages that set a field to a value │ model.state becomes "Error"       │ Catching a specific transition │
 └──────────────────────────────────────┴───────────────────────────────────┴────────────────────────────────┘
 Interaction patterns:
 - Checkbox in filter bar: "Only show messages with state changes"
 - Click on field in state tree: Click on elevatorPosition → "Watch this field"
 - Watch panel: A dedicated panel showing watched fields and when they changed
 - Field breadcrumb click: When viewing state, click on path segment to add watch

 3. By Effects Produced

 Use case: "When did we make HTTP requests?" or "What triggered that command?"
 ┌────────────────────────────────────────────────┬───────────────────────────────┬───────────────────────────────┐
 │                  Filter Type                   │            Example            │          When Useful          │
 ├────────────────────────────────────────────────┼───────────────────────────────┼───────────────────────────────┤
 │ Messages producing any effect                  │ Has effects                   │ Understanding command flow    │
 ├────────────────────────────────────────────────┼───────────────────────────────┼───────────────────────────────┤
 │ Messages producing specific effect             │ Http.Request effects          │ Debugging API interactions    │
 ├────────────────────────────────────────────────┼───────────────────────────────┼───────────────────────────────┤
 │ Messages producing effect to specific endpoint │ Effects containing /api/users │ Debugging a specific API call │
 └────────────────────────────────────────────────┴───────────────────────────────┴───────────────────────────────┘
 Interaction patterns:
 - Effect type filter dropdown: Select from effect types seen in log
 - Click on effect in details panel: "Show all messages with this effect type"

 4. By Time

 Use case: "Something went wrong around 10:32:15"
 ┌──────────────────────┬────────────────────────────────────┬───────────────────────────────┐
 │     Filter Type      │              Example               │          When Useful          │
 ├──────────────────────┼────────────────────────────────────┼───────────────────────────────┤
 │ Time range           │ Messages from 10:32:00 to 10:32:30 │ Isolating an incident window  │
 ├──────────────────────┼────────────────────────────────────┼───────────────────────────────┤
 │ Relative time        │ Last 100 messages before error     │ Working backward from symptom │
 ├──────────────────────┼────────────────────────────────────┼───────────────────────────────┤
 │ Time around selected │ ±50 messages around selection      │ Exploring context             │
 └──────────────────────┴────────────────────────────────────┴───────────────────────────────┘
 Interaction patterns:
 - Range slider on timeline: Drag handles to set time window
 - Keyboard: [ and ] to narrow/expand time window around selection
 - Click on timestamp: Set as start/end of range

 5. By Bookmarks/Annotations

 Use case: "I found 5 interesting messages, I want to compare them"
 ┌───────────────────────────┬──────────────────────────────┬─────────────────────────────────┐
 │        Filter Type        │           Example            │           When Useful           │
 ├───────────────────────────┼──────────────────────────────┼─────────────────────────────────┤
 │ Show only bookmarked      │ User-marked messages         │ Comparing specific events       │
 ├───────────────────────────┼──────────────────────────────┼─────────────────────────────────┤
 │ Show bookmarked + context │ Bookmarks ± N messages       │ Understanding around key events │
 ├───────────────────────────┼──────────────────────────────┼─────────────────────────────────┤
 │ Filter by annotation/tag  │ Messages tagged "suspicious" │ Organizing investigation        │
 └───────────────────────────┴──────────────────────────────┴─────────────────────────────────┘
 Interaction patterns:
 - Star/bookmark icon on messages: Click to bookmark
 - Keyboard shortcut: B to bookmark selected message
 - Bookmark panel: List of bookmarks with jump-to
 - Filter toggle: "Show only bookmarked"

 Interaction Modes

 Mode 1: Additive Filtering (Filter Bar)

 Traditional filter UI - user explicitly builds filter criteria.

 +------------------------------------------+
 | [ElevatorSubsystem.*    ] [+ Include]    |
 | [UpdateTime             ] [+ Exclude]    |
 +------------------------------------------+
 | Active filters:                          |
 | [x] Include: ElevatorSubsystem.*    [X]  |
 | [x] Exclude: UpdateTime             [X]  |
 | [x] Only state changes              [X]  |
 +------------------------------------------+

 Pros: Explicit, predictable, composable
 Cons: Requires knowing what to filter for

 Mode 2: Contextual Actions (Right-Click)

 Filter actions available where relevant - on messages, fields, effects.

 Right-click on "ElevatorSubsystem.Message.UpdatePosition":
 ┌──────────────────────────────────────┐
 │ Show only this message type          │
 │ Show only ElevatorSubsystem.*        │
 │ ──────────────────────────────────── │
 │ Hide this message type               │
 │ Hide all ElevatorSubsystem.*         │
 │ ──────────────────────────────────── │
 │ Bookmark this message                │
 └──────────────────────────────────────┘

 Right-click on field "model.elevator.position" in state tree:
 ┌──────────────────────────────────────┐
 │ Watch this field for changes         │
 │ Show messages that change this field │
 │ Copy path                            │
 └──────────────────────────────────────┘

 Pros: Discoverable, quick, contextually relevant
 Cons: Hidden until right-click, requires knowing to look

 Mode 3: Smart Suggestions

 System suggests useful filters based on patterns in the data.

 +------------------------------------------+
 | Suggested filters:                       |
 | [Apply] Hide UpdateTime (67% of messages)|
 | [Apply] Hide InputSubsystem.* (23%)      |
 | [Apply] Only ElevatorSubsystem.* (5%)    |
 +------------------------------------------+

 Pros: Helps user discover high-frequency noise
 Cons: May suggest wrong things, adds complexity

 Mode 4: Quick Toggles

 Single-click toggles for common operations.

 Message list item:
 ┌────────────────────────────────────────────┐
 │ [👁] ElevatorSubsystem.Message.Update...
 │      #1234 · 10:32:15                      │
 └────────────────────────────────────────────┘
         ↑
    Eye icon: click to toggle visibility of this message type

 Or as a hover action:
 ┌────────────────────────────────────────────┐
 │ ElevatorSubsystem.Message.Update...  [H][O]│
 │ #1234 · 10:32:15                           │
 └────────────────────────────────────────────┘
                                          ↑  ↑
                                    Hide  Only

 Pros: Very fast once learned
 Cons: Clutters UI, non-obvious meaning

 Mode 5: Field Watches (Inspector-Style)

 Dedicated panel for monitoring specific fields, like a debugger's watch window.

 ┌─ Watches ─────────────────────────────────┐
 │ model.elevator.position                   │
 │   Current: 42.5                           │
 │   Changed: #1234, #1256, #1289            │
 │                                           │
 │ model.state                               │
 │   Current: "Running"                      │
 │   Changed: #1200 (Idle → Running)         │
 │                                           │
 │ [+ Add watch...]                          │
 └───────────────────────────────────────────┘

 Clicking a "Changed" message jumps to it. Can filter message list to "only messages that changed a watched field."

 Pros: Familiar to debugger users, persistent across exploration
 Cons: Requires screen space, additional concept to learn

 Compound Filters

 How should multiple filters combine?

 Option A: All filters AND together

 Message must pass ALL filters. Simple but restrictive.

 Option B: Include/Exclude logic

 1. If any include filters exist, message must match at least one
 2. Then, message must NOT match any exclude filter
 3. Additional filters (only changed, time range) apply on top

 This matches how users think: "Show me elevator messages, but hide the noisy UpdateTime ones"

 Option C: Filter groups with explicit logic

 Power-user feature: create filter groups with AND/OR/NOT.

 (Include: ElevatorSubsystem.* OR Include: SwerveSubsystem.*)
 AND NOT (Exclude: UpdateTime)
 AND (Only changed)

## Questions for Further Exploration

 1. Should filters persist across file loads, or reset with each file?
 2. Should there be "filter presets" for common patterns?
 3. How should the filtered count be shown? (e.g., "82 of 247 messages")
 4. Should filtered-out messages be completely hidden or shown grayed out?
 5. Should keyboard shortcuts be a priority for power users?