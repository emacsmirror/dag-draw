# dag-draw.el

**Drawing graphs that don't suck.**

## The Problem

Ever tried to draw a flowchart? You spend hours moving boxes around, trying to untangle crossing lines, and the result still looks like spaghetti. The same thing happens when visualizing:

- Project dependencies ("Task A must finish before B and C")
- Course prerequisites ("You need Calc I before Calc II")
- File dependencies ("main.c includes utils.h")
- Workflow steps ("Design → Build → Test → Deploy")

**The manual approach is painful.** You fiddle with positions, redraw arrows, realize you need to move everything down to make room... and start over.

## The Solution

dag-draw.el does the hard part for you. You focus on the **structure** (what connects to what), and it handles the **layout** (where everything goes).

```elisp
;; You write this:
(dag-draw-add-node graph 'design "Design")
(dag-draw-add-node graph 'build "Build")
(dag-draw-add-node graph 'test "Test")
(dag-draw-add-edge graph 'design 'build)
(dag-draw-add-edge graph 'design 'test)
(dag-draw-add-edge graph 'build 'test)

;; It produces this:
```
```
┌──────┐
│Design│
└───┬──┘
    │
    ├────────────────┐
    │                │
    ▼                ▼
┌───────┐        ┌──────┐
│Build  │───────▶│Test  │
└───────┘        └──────┘
```

**No manual positioning. No tangled arrows. Just clean, hierarchical layouts.**

## Your First Graph (30 Seconds)

Let's draw something simple—a three-step workflow:

```elisp
(require 'dag-draw)

;; 1. Create a graph
(setq my-graph (dag-draw-create-graph))

;; 2. Add your nodes
(dag-draw-add-node my-graph 'start "Start Here")
(dag-draw-add-node my-graph 'middle "Do Work")
(dag-draw-add-node my-graph 'done "Finish")

;; 3. Connect them
(dag-draw-add-edge my-graph 'start 'middle)
(dag-draw-add-edge my-graph 'middle 'done)

;; 4. Layout and render
(dag-draw-layout-graph my-graph)
(dag-draw-render-graph my-graph 'ascii)
```

**Output:**
```
┌────────────┐
│Start Here  │
└──────┬─────┘
       │
       ▼
┌────────────┐
│Do Work     │
└──────┬─────┘
       │
       ▼
┌────────────┐
│Finish      │
└────────────┘
```

**Congratulations!** You just created a professionally-laid-out graph. You specified the structure; dag-draw handled everything else.

## Creating Graphs from Data

**Build entire graphs from data structures in one call.** When you have graph data from a database, configuration file, or external source, imperatively adding nodes and edges one-by-one is tedious. Batch creation lets you describe the whole graph declaratively.

**The problem:** You have a list of tasks with dependencies in a database. Fetching and converting to a graph requires 50+ lines of `dag-draw-add-node` and `dag-draw-add-edge` calls. Copy-paste errors are easy.

**The solution:** `dag-draw-create-from-spec` accepts a complete graph specification as data. One function call builds the entire graph.

**For:** Anyone generating graphs from external data, configuration files, or programmatic sources.

### Your First Batch Graph (30 Seconds)

Here's the same three-step workflow, but built from data:

```elisp
(require 'dag-draw)

;; One function call creates the entire graph
(setq my-graph (dag-draw-create-from-spec
                 :nodes '((start :label "Start Here")
                          (middle :label "Do Work")
                          (done :label "Finish"))
                 :edges '((start middle)
                          (middle done))))

;; Layout and render (same as before)
(dag-draw-layout-graph my-graph)
(dag-draw-render-graph my-graph 'ascii)
```

**Output:**
```
┌────────────┐
│Start Here  │
└──────┬─────┘
       │
       ▼
┌────────────┐
│Do Work     │
└──────┬─────┘
       │
       ▼
┌────────────┐
│Finish      │
└────────────┘
```

**That's it!** Same result, but the graph structure is data you can store, generate, or manipulate.

### Understanding Batch Creation

Think of **JSON configuration files** or **database query results**. You don't build JSON imperatively with setter functions—you create the whole structure at once.

Batch creation works the same way:

**Imperative (step-by-step):**
```elisp
(setq graph (dag-draw-create-graph))
(dag-draw-add-node graph 'a "Task A")
(dag-draw-add-node graph 'b "Task B")
(dag-draw-add-node graph 'c "Task C")
(dag-draw-add-edge graph 'a 'b)
(dag-draw-add-edge graph 'b 'c)
;; 8 lines for 3 nodes, 2 edges
```

**Declarative (all at once):**
```elisp
(setq graph (dag-draw-create-from-spec
              :nodes '((a :label "Task A")
                       (b :label "Task B")
                       (c :label "Task C"))
              :edges '((a b) (b c))))
;; 5 lines for same graph
```

**Why this matters:**
- **Data-driven:** Graph structure is data, not code
- **Composable:** Build node/edge lists programmatically
- **Readable:** See the whole structure at a glance
- **From external sources:** Easy to convert from databases, APIs, files

### Tutorial: Generating Graphs from Data

**Scenario:** You have task data in a list and want to visualize dependencies.

```elisp
;; Task data from database/API
(setq task-data
  '((1 "Research" nil)           ; (id label dependencies)
    (2 "Design" (1))
    (3 "Implementation" (2))
    (4 "Testing" (3))
    (5 "Deployment" (4))))

;; Convert to graph spec
(setq nodes
  (mapcar (lambda (task)
            (let ((id (car task))
                  (label (cadr task)))
              (list id :label label)))
          task-data))

(setq edges
  (cl-loop for task in task-data
           for id = (car task)
           for deps = (caddr task)
           append (mapcar (lambda (dep) (list dep id)) deps)))

;; Create graph from spec
(setq project-graph (dag-draw-create-from-spec
                      :nodes nodes
                      :edges edges))

(dag-draw-layout-graph project-graph)
(dag-draw-render-graph project-graph 'ascii)
```

**Output:**
```
┌──────────┐
│Research  │
└─────┬────┘
      │
      ▼
┌────────┐
│Design  │
└────┬───┘
     │
     ▼
┌────────────────┐
│Implementation  │
└────────┬───────┘
         │
         ▼
┌─────────┐
│Testing  │
└────┬────┘
     │
     ▼
┌────────────┐
│Deployment  │
└────────────┘
```

**The pattern:** Fetch data → Transform to spec format → Create graph. Works with any data source.

### Tutorial: Batch Creation with Visual Properties

Visual properties work seamlessly with batch creation:

```elisp
(setq status-graph (dag-draw-create-from-spec
                     :nodes '((todo :label "TODO"
                                    :svg-fill "#e0e0e0")
                              (active :label "In Progress"
                                      :ascii-marker "→ "
                                      :ascii-highlight t
                                      :svg-fill "#ffd700"
                                      :svg-stroke "#ff8c00"
                                      :svg-stroke-width 2)
                              (done :label "Done"
                                    :ascii-marker "✓ "
                                    :svg-fill "#90ee90"
                                    :svg-stroke "#228b22"
                                    :svg-stroke-width 2))
                     :edges '((todo active)
                              (active done))))

(dag-draw-layout-graph status-graph)
(dag-draw-render-graph status-graph 'ascii)
```

**ASCII Output:**
```
┌──────┐
│TODO  │
└───┬──┘
    │
    ▼
╔═══════════════╗
║→ In Progress  ║
╚═══════╬═══════╝
        │
        ▼
┌─────────────┐
│✓ Done       │
└─────────────┘
```

**SVG Output:** Colors and borders render as specified.

**Combining status data with graph generation:**
```elisp
(defun create-status-graph (tasks)
  "Create graph with status-based visual properties."
  (let ((nodes
         (mapcar (lambda (task)
                   (let ((id (plist-get task :id))
                         (label (plist-get task :label))
                         (status (plist-get task :status)))
                     (append
                      (list id :label label)
                      (cond
                       ((eq status 'done)
                        '(:ascii-marker "✓ " :svg-fill "#90ee90"))
                       ((eq status 'active)
                        '(:ascii-highlight t :svg-fill "#ffd700"))
                       (t '(:svg-fill "#e0e0e0"))))))
                 tasks))
        (edges
         (cl-loop for task in tasks
                  for id = (plist-get task :id)
                  for deps = (plist-get task :dependencies)
                  append (mapcar (lambda (dep) (list dep id)) deps))))
    (dag-draw-create-from-spec :nodes nodes :edges edges)))
```

Now visual properties update automatically based on task status.

### Tutorial: Edge Attributes

Edges can have attributes too—weights, labels, or custom properties:

```elisp
(setq workflow (dag-draw-create-from-spec
                 :nodes '((start :label "Start")
                          (critical :label "Critical Path")
                          (optional :label "Optional Step")
                          (end :label "End"))
                 :edges '((start critical :weight 10)              ; High priority
                          (start optional :weight 1)               ; Low priority
                          (critical end :weight 10 :label "required")
                          (optional end :weight 1))))

(dag-draw-layout-graph workflow)
(dag-draw-render-graph workflow 'svg)
```

Edge format: `(from-id to-id &rest attributes)`
- Simple: `(a b)`
- With weight: `(a b :weight 10)`
- Multiple attrs: `(a b :weight 10 :label "requires")`

Higher weights pull nodes closer together in the layout.

### Common Patterns

**Pattern 1: Data-Driven Graphs**
```elisp
;; Generate nodes/edges from external data
(setq nodes (mapcar #'task-to-node-spec external-tasks))
(setq edges (mapcar #'dependency-to-edge-spec external-deps))
(dag-draw-create-from-spec :nodes nodes :edges edges)
```

**Pattern 2: Configuration-Based Graphs**
```elisp
;; Store graph structure in configuration
(defvar my-workflow-config
  '(:nodes ((review :label "Code Review")
            (test :label "Testing")
            (deploy :label "Deploy"))
    :edges ((review test) (test deploy))))

(dag-draw-create-from-spec
  :nodes (plist-get my-workflow-config :nodes)
  :edges (plist-get my-workflow-config :edges))
```

**Pattern 3: Template-Based Graphs**
```elisp
;; Define reusable graph templates
(defun create-pipeline-graph (stages)
  (let ((nodes (mapcar (lambda (stage)
                         (list (car stage) :label (cdr stage)))
                       stages))
        (edges (cl-loop for i from 0 below (1- (length stages))
                        collect (list (car (nth i stages))
                                      (car (nth (1+ i) stages))))))
    (dag-draw-create-from-spec :nodes nodes :edges edges)))

;; Use template
(create-pipeline-graph '((build . "Build")
                         (test . "Test")
                         (deploy . "Deploy")))
```

### When to Use Batch Creation

**Use batch creation when:**
- ✅ Graph data comes from external sources (DB, API, files)
- ✅ Building graphs programmatically from data structures
- ✅ Graph structure is configuration (stored in variables/files)
- ✅ Want to see entire graph structure at a glance
- ✅ Generating graphs in loops or from templates

**Use imperative API when:**
- ✅ Building graphs interactively in code
- ✅ Graph structure is built incrementally with complex logic
- ✅ Adding/removing nodes dynamically based on conditions
- ✅ Modifying existing graphs (use `dag-draw-add-node` on existing graph)

**Rule of thumb:** If your graph structure fits in a data literal, use batch creation. If it's built with complex conditionals and loops, use imperative.

## Understanding Graph Layout

### What's a Directed Graph?

Think of a **family tree**, but where arrows show direction. Or a **course prerequisite chart** where arrows point from required courses to courses that need them.

More formally:
- **Nodes** (boxes) represent things: tasks, people, files, concepts
- **Edges** (arrows) represent relationships: "A depends on B", "A comes before B"

### What's "Acyclic"?

It means **no loops**. You can't follow the arrows and end up back where you started.

Valid (acyclic):
```
A → B → C
```

Invalid (has a cycle):
```
A → B → C → A  (cycle!)
```

**Why does this matter?** Acyclic graphs represent **hierarchies** and **dependencies**. You can't be your own ancestor. A task can't depend on itself. These graphs have a natural "top-to-bottom" flow.

### Why Does Layout Matter?

Look at the same graph with bad vs good layout:

**Bad (manual) layout:**
```
┌─┐        ┌─┐
│A│───┐    │C│
└─┘   │    └─┘
      │     │
      │  ┌──┘
      │  │
      ▼  ▼
      ┌─┐
      │D│◀───┐
      └─┘    │
             │
      ┌─┐    │
      │B│────┘
      └─┘
```
Crossing lines! Hard to follow!

**Good (dag-draw) layout:**
```
┌─┐  ┌─┐
│A│  │B│
└┬┘  └┬┘
 │    │
 ├────┴───┐
 │        │
 ▼        ▼
┌─┐      ┌─┐
│C│      │D│
└─┘      └─┘
```
Clear hierarchy! Easy to read!

**This is what dag-draw does:** It takes your messy graph and organizes it beautifully.

## How It Works: From Nodes to Diagrams

When you call `(dag-draw-layout-graph graph)`, four things happen automatically:

### Pass 1: Ranking (Vertical Levels)

**Goal:** Figure out which nodes go on which "level."

```
Level 0:  [A] [B]        (no dependencies)
Level 1:  [C] [D]        (depend on level 0)
Level 2:  [E]            (depends on level 1)
```

**Why:** This creates the top-to-bottom flow. Nodes that nothing depends on go at the top. Everything flows downward.

### Pass 2: Ordering (Horizontal Positions)

**Goal:** Within each level, arrange nodes to minimize crossing arrows.

```
Level 1:  [C] [D]    (fewer crossings)
   vs
Level 1:  [D] [C]    (more crossings)
```

**Why:** Crossing lines are confusing. The algorithm tries hundreds of arrangements to find one with minimal crossings.

### Pass 3: Positioning (Exact Coordinates)

**Goal:** Calculate precise X,Y positions that:
- Keep enough space between nodes
- Align connected nodes nicely
- Make edges as short as practical

**Why:** "Minimize crossings" isn't enough. We need actual positions for rendering.

### Pass 4: Splines (Smooth Edges)

**Goal:** Draw edges as smooth curves (not just straight lines) that avoid overlapping nodes.

**Why:** Straight lines look harsh. Smooth curves look professional and are easier to follow.

**The Result:** Your nodes and edges are now positioned beautifully. Time to render!

## Choosing Your Output: ASCII vs SVG

dag-draw supports two output formats. **When do you use each?**

### ASCII: Quick and Portable

```elisp
(dag-draw-render-graph my-graph 'ascii)
```

**Best for:**
- Quick previews in your terminal
- Including in source code comments
- Email or plain-text documentation
- README files (like this one!)
- Debugging layouts

**Characteristics:**
- Uses box-drawing characters (┌─┐│└┘├┤┬┴┼)
- Looks good in monospace fonts
- Copy-paste anywhere
- Instant visual feedback

### SVG: Publication Quality

```elisp
(dag-draw-render-graph my-graph 'svg)
```

**Best for:**
- Documentation websites
- Presentations
- Scalable diagrams (zoom without pixelation)
- Custom styling (colors, fonts)
- Interactive web applications

**Characteristics:**
- Smooth Bézier curves
- Crisp at any size
- Can embed in HTML
- Professional appearance

**Rule of thumb:** Use ASCII for quick checks, SVG for final output.

## Customizing Your Graphs

Now that you understand the basics, let's make graphs look the way you want.

### Adjusting Spacing

Graph too cramped?

```elisp
;; More space between nodes horizontally
(setq dag-draw-default-node-separation 30)

;; More space between vertical levels
(setq dag-draw-default-rank-separation 50)

(dag-draw-layout-graph my-graph)
```

### Adding Visual Attributes

Want colored nodes or custom shapes?

```elisp
(dag-draw-add-node my-graph 'critical "Critical Task"
  (ht ("color" "red")
      ("shape" "diamond")))

(dag-draw-add-node my-graph 'optional "Optional"
  (ht ("color" "gray")
      ("style" "dashed")))
```

These attributes are used by the SVG renderer for styling.

### Edge Weights

Some dependencies are stronger than others:

```elisp
;; High-priority dependency
(dag-draw-add-edge my-graph 'a 'b
  (ht ("weight" 10)))

;; Low-priority optional dependency
(dag-draw-add-edge my-graph 'a 'c
  (ht ("weight" 1)))
```

Higher weights pull nodes closer together.

## Visual Properties: Making Nodes Stand Out

**Show status, priority, or categories through visual styling.** When your graph represents a workflow, some nodes are done, some are in progress, some are blocked. Or you're showing a system where different nodes have different importance levels. Visual properties let you color-code meaning.

**The problem:** You have a project with 15 tasks. Three are done (green), two are critical (red), one is currently active (highlighted). How do you show this at a glance without reading every label?

**The solution:** Per-node visual attributes. Each node can have its own colors, borders, and markers. Your graph becomes a visual status dashboard where meaning is immediately apparent.

**For:** Anyone using graphs to show status (task boards, CI/CD pipelines, health monitoring) or to categorize nodes (severity levels, types, ownership).

### Your First Visual Properties (30 Seconds)

Let's add a simple status marker to show which tasks are done:

```elisp
(require 'dag-draw)
(require 'ht)  ;; Hash table library

;; Create graph
(setq tasks (dag-draw-create-graph))

;; Regular task (no marker)
(dag-draw-add-node tasks 'start "Start")

;; Completed task (checkmark marker)
(dag-draw-add-node tasks 'done "Research"
  (ht (:ascii-marker "✓ ")))

(dag-draw-add-edge tasks 'start 'done)
(dag-draw-layout-graph tasks)
(dag-draw-render-graph tasks 'ascii)
```

**Output:**
```
┌───────┐
│Start  │
└───┬───┘
    │
    ▼
┌─────────────┐
│✓ Research   │
└─────────────┘
```

**That's it.** The checkmark instantly shows "Research" is complete. No need to read the label to know status.

### Understanding Visual Properties

Think of a **project management board** like Trello or Jira. Cards have colored labels (red = urgent, green = done, yellow = in progress). You can scan the board and instantly see status by color, without reading text.

Graph visual properties work the same way:

**Without visual properties:**
```
┌──────────┐
│Task A    │  (Status: unknown)
└────┬─────┘
     │
     ▼
┌──────────┐
│Task B    │  (Priority: unknown)
└──────────┘
```
All tasks look identical. You must read labels or check elsewhere to learn status/priority.

**With visual properties:**
```
┌──────────┐
│Task A    │  (Gray = normal)
└────┬─────┘
     │
     ▼
╔══════════╗
║✓ Task B  ║  (Double border + check = DONE)
╚══════════╝
```
**Or in SVG with colors:**
- Green fill = done
- Orange border = in progress
- Red fill = blocked
- Blue glow = currently selected

**Why this matters:**
- **Instant status recognition** - No reading required
- **Visual scanning** - Find blocked tasks in a 20-node graph instantly
- **Meaningful color** - Red/yellow/green conveys urgency without text
- **Combined signals** - Markers + colors + borders = rich information

### Tutorial: Status Tracking with Markers

**Scenario:** You're building a tutorial system. You want to show which lessons are completed as the user progresses.

```elisp
(setq tutorial (dag-draw-create-graph))

;; Completed lessons get checkmarks
(dag-draw-add-node tutorial 'intro "Introduction"
  (ht (:ascii-marker "✓ ")))

(dag-draw-add-node tutorial 'basics "Basic Concepts"
  (ht (:ascii-marker "✓ ")))

;; Current lesson gets an arrow
(dag-draw-add-node tutorial 'advanced "Advanced Topics"
  (ht (:ascii-marker "→ ")))

;; Future lessons have no marker
(dag-draw-add-node tutorial 'expert "Expert Level")

(dag-draw-add-edge tutorial 'intro 'basics)
(dag-draw-add-edge tutorial 'basics 'advanced)
(dag-draw-add-edge tutorial 'advanced 'expert)

(dag-draw-layout-graph tutorial)
(dag-draw-render-graph tutorial 'ascii)
```

**Output:**
```
┌─────────────────┐
│✓ Introduction   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│✓ Basic Concepts │
└────────┬────────┘
         │
         ▼
┌───────────────────────┐
│→ Advanced Topics      │
└──────────┬────────────┘
           │
           ▼
┌───────────────────┐
│Expert Level       │
└───────────────────┘
```

**Instant visual understanding:**
- ✓ = Done (lessons 1-2)
- → = Current (lesson 3)
- (no marker) = Not started (lesson 4)

User sees progress at a glance. No need to check a separate progress tracker.

### Tutorial: Priority Levels with Highlighting

**Scenario:** CI/CD pipeline status board showing build health.

```elisp
(setq pipeline (dag-draw-create-graph))

;; Successful steps (normal)
(dag-draw-add-node pipeline 'tests "Unit Tests")

;; Warning state (highlight with double-line border)
(dag-draw-add-node pipeline 'build "Build"
  (ht (:ascii-highlight t)))

;; Failed step (highlight + marker)
(dag-draw-add-node pipeline 'deploy "Deploy"
  (ht (:ascii-highlight t)
      (:ascii-marker "✗ ")))

(dag-draw-add-edge pipeline 'tests 'build)
(dag-draw-add-edge pipeline 'build 'deploy)

(dag-draw-layout-graph pipeline)
(dag-draw-render-graph pipeline 'ascii)
```

**Output:**
```
┌────────────┐
│Unit Tests  │  ← Passed (normal border)
└──────┬─────┘
       │
       ▼
╔════════════╗
║Build       ║  ← Warning (double border)
╚══════╬═════╝
       │
       ▼
╔════════════╗
║✗ Deploy    ║  ← Failed (double border + X)
╚════════════╝
```

**Visual signals combined:**
- Single border = Success
- Double border = Attention needed
- ✗ marker = Failure
- ✓ marker = Success (if you want to mark it explicitly)

### SVG Visual Properties: Publication-Quality Styling

ASCII markers work great in terminals, but SVG supports full color. Let's use the same pipeline example with SVG styling:

```elisp
(setq pipeline (dag-draw-create-graph))

;; Success: light green background
(dag-draw-add-node pipeline 'tests "Unit Tests"
  (ht (:svg-fill "#90ee90")))

;; Warning: orange border, yellow background
(dag-draw-add-node pipeline 'build "Build"
  (ht (:svg-fill "#ffd700")
      (:svg-stroke "#ff8c00")
      (:svg-stroke-width 2)))

;; Failed: red background, dark red border
(dag-draw-add-node pipeline 'deploy "Deploy"
  (ht (:svg-fill "#ff4444")
      (:svg-stroke "#cc0000")
      (:svg-stroke-width 3)))

(dag-draw-add-edge pipeline 'tests 'build)
(dag-draw-add-edge pipeline 'build 'deploy)

(dag-draw-layout-graph pipeline)
(dag-draw-render-graph pipeline 'svg)
```

The SVG output shows:
- **Unit Tests** - Light green fill (success color)
- **Build** - Yellow fill with orange border (warning colors)
- **Deploy** - Red fill with dark red thick border (error colors)

Colors convey meaning instantly: Green = good, Yellow = caution, Red = problem.

### Tutorial: Building a Status Dashboard

**Scenario:** Task management system with different task states.

```elisp
(setq project (dag-draw-create-graph))

;; TODO: Gray background
(dag-draw-add-node project 'research "Research"
  (ht (:svg-fill "#e0e0e0")))

;; IN PROGRESS: Gold background, orange border
(dag-draw-add-node project 'implement "Implementation"
  (ht (:svg-fill "#ffd700")
      (:svg-stroke "#ff8c00")
      (:svg-stroke-width 2)))

;; DONE: Light green background, green border
(dag-draw-add-node project 'test "Testing"
  (ht (:svg-fill "#90ee90")
      (:svg-stroke "#228b22")
      (:svg-stroke-width 2)))

(dag-draw-add-edge project 'research 'implement)
(dag-draw-add-edge project 'implement 'test)

(dag-draw-layout-graph project)

;; Render for documentation (SVG)
(with-temp-file "project-status.svg"
  (insert (dag-draw-render-graph project 'svg)))
```

**The result:** A color-coded status dashboard where:
- Gray nodes = TODO (not started)
- Gold nodes with orange borders = IN PROGRESS (active work)
- Green nodes with green borders = DONE (completed)

Stakeholders can view the SVG and instantly understand project status without reading any text.

### Tutorial: Advanced SVG Styling

**Scenario:** Interactive system diagram where nodes need tooltips, semi-transparent states, and color-coded labels.

Sometimes you need more than fill colors and borders:
- **Tooltips** show extra info on hover (SVG `<title>` elements)
- **Opacity** shows faded/inactive states
- **Text colors** distinguish node categories

```elisp
(setq system (dag-draw-create-graph))

;; Active service: tooltip shows uptime, solid colors
(dag-draw-add-node system 'api "API Gateway"
  (ht (:svg-fill "#90ee90")
      (:svg-tooltip "Status: Running\nUptime: 99.9%\nLast deploy: 2025-11-15")))

;; Degraded service: tooltip shows issue, semi-transparent to show reduced capacity
(dag-draw-add-node system 'cache "Cache Layer"
  (ht (:svg-fill "#ffd700")
      (:svg-fill-opacity 0.6)
      (:svg-tooltip "Status: Degraded\nMemory: 85% full\nRestart needed")))

;; Critical service: red text to emphasize importance
(dag-draw-add-node system 'db "Database"
  (ht (:svg-fill "#ffe0e0")
      (:svg-text-color "#cc0000")
      (:svg-stroke "#cc0000")
      (:svg-stroke-width 2)
      (:svg-tooltip "Status: Critical\nConnections: 95/100\nAlert: High load")))

(dag-draw-add-edge system 'api 'cache)
(dag-draw-add-edge system 'cache 'db)

(dag-draw-layout-graph system)
(dag-draw-render-graph system 'svg)
```

**The result:**
- **API Gateway** - Green background, hover shows "Status: Running..."
- **Cache Layer** - Semi-transparent yellow (60% opacity) shows degraded state
- **Database** - Red text and red border emphasize critical status, hover shows alert details

**When to use these properties:**
- `:svg-tooltip` - Add context without cluttering the diagram (hover to see details)
- `:svg-fill-opacity` - Show inactive, disabled, or degraded states (0.0-1.0)
- `:svg-text-color` - Color-code node labels (red = error, green = success, blue = info)

### Combining Visual Properties with Selection

You can use both attribute-based styling AND the `selected` parameter:

```elisp
;; All tasks have status colors
(dag-draw-add-node workflow 'a "Task A" (ht (:svg-fill "#90ee90")))  ; Done
(dag-draw-add-node workflow 'b "Task B" (ht (:svg-fill "#ffd700")))  ; Active
(dag-draw-add-node workflow 'c "Task C" (ht (:svg-fill "#e0e0e0")))  ; TODO

(dag-draw-layout-graph workflow)

;; Render with Task B selected (adds blue glow)
(dag-draw-render-graph workflow 'svg 'b)
```

Task B now has:
- Yellow fill (from `:svg-fill` attribute) = "In Progress" status
- Blue glow (from `selected` parameter) = "Currently viewing"

**Use both when:**
- Attributes show **state** (done/active/todo, passed/failed, etc.)
- Selection shows **focus** (which one user is looking at right now)

### Common Visual Property Patterns

**Pattern 1: Traffic Light Colors (Status)**
```elisp
;; Good/Warning/Error color scheme
(ht (:svg-fill "#90ee90"))  ; Green = Good
(ht (:svg-fill "#ffd700")   ; Yellow = Warning
    (:svg-stroke "#ff8c00"))
(ht (:svg-fill "#ff4444")   ; Red = Error
    (:svg-stroke "#cc0000")
    (:svg-stroke-width 3))
```

**Pattern 2: Progress Markers (Completion)**
```elisp
;; ASCII markers for tutorial/wizard progress
(ht (:ascii-marker "✓ "))  ; Completed step
(ht (:ascii-marker "→ "))  ; Current step
(ht (:ascii-marker "○ "))  ; Future step (optional)
```

**Pattern 3: Severity Levels (Priority)**
```elisp
;; Increasing visual weight for urgency
(ht)                        ; Normal (default styling)
(ht (:svg-stroke-width 2))  ; Important (thicker border)
(ht (:svg-stroke-width 3)   ; Critical (thick border + red)
    (:svg-stroke "#ff0000")
    (:svg-fill "#ffe0e0"))
```

**Pattern 4: Category Colors (Types)**
```elisp
;; Different node types in system architecture
(ht (:svg-fill "#e3f2fd"))  ; Blue = Database
(ht (:svg-fill "#fff3e0"))  ; Orange = Service
(ht (:svg-fill "#f3e5f5"))  ; Purple = Queue
(ht (:svg-fill "#e8f5e9"))  ; Green = Cache
```

### When to Use Visual Properties

**Use visual properties when:**
- ✅ Nodes have distinct states (todo/doing/done, pass/fail, healthy/degraded)
- ✅ Different nodes have different priorities or severity levels
- ✅ You're building a dashboard or monitoring view
- ✅ Color-coding aids understanding (red=error, green=success)
- ✅ You want to show progress through a workflow visually

**Don't use visual properties when:**
- ❌ All nodes are the same type/status (no differentiation needed)
- ❌ Graph is purely structural documentation (no state to show)
- ❌ Terminal doesn't support unicode/colors (use `selected` parameter instead)

**Rule of thumb:** If you'd use colored sticky notes on a whiteboard to show status, use visual properties in your graph.

## Highlighting Nodes: Show "You Are Here"

**Visual emphasis for the current node in your workflow.** When stepping through a tutorial, debugging a workflow, or showing status, one node needs to stand out.

**The problem:** You have a graph showing 10 workflow steps. The user is on step 5. How do they know which box represents "where they are right now"?

**The solution:** Node selection highlighting. One node gets special visual treatment. In ASCII it's double-line borders. In SVG it glows. The rest stay normal.

**For:** Anyone building interactive graph viewers, step-by-step tutorials, or status dashboards.

### Your First Highlighted Node (30 Seconds)

Already have a graph? Add one parameter to see the difference:

```elisp
;; Before: Normal rendering
(dag-draw-render-graph my-graph 'ascii)

;; After: Highlights 'current-step
(dag-draw-render-graph my-graph 'ascii 'current-step)
```

**That's it.** The node with ID `'current-step` now stands out visually. Everyone else stays normal.

Try it now with any graph you already have. Pick any node ID, add it as the third parameter.

### Understanding "You Are Here" Marking

Think of a shopping mall directory map. There's a red arrow saying "YOU ARE HERE" so you know your location among dozens of stores.

Graph highlighting works the same way:

**Without highlighting:**
```
┌─────┐
│Step1│
└──┬──┘
   │
   ▼
┌─────┐
│Step2│  ← User is here, but which box is it?
└──┬──┘
   │
   ▼
┌─────┐
│Step3│
└─────┘
```

All three boxes look identical. User can't tell where they are.

**With highlighting:**
```
┌─────┐
│Step1│
└──┬──┘
   │
   ▼
╔═════╗
║Step2║  ← Double borders = "YOU ARE HERE"
╚══╬══╝
   │
   ▼
┌─────┐
│Step3│
└─────┘
```

Now it's obvious. Step2 has thick double-line borders (`╔═║╗`) instead of thin single-line (`┌─│┐`).

**Why this matters:**
- User knows their location instantly
- No need to read labels to find current position
- Works even with large graphs (20+ nodes)

### Tutorial: Building a Step-Through Guide

Let's build something real: a wizard that shows the user which step they're on.

**Scenario:** Software installation has 4 steps. We want to show progress.

```elisp
;; Define the workflow
(setq install-graph (dag-draw-create-graph))
(dag-draw-add-node install-graph 'download "Download")
(dag-draw-add-node install-graph 'extract "Extract Files")
(dag-draw-add-node install-graph 'configure "Configure")
(dag-draw-add-node install-graph 'install "Install")

(dag-draw-add-edge install-graph 'download 'extract)
(dag-draw-add-edge install-graph 'extract 'configure)
(dag-draw-add-edge install-graph 'configure 'install)

(dag-draw-layout-graph install-graph)

;; User is on step 2 (extract)
(dag-draw-render-graph install-graph 'ascii 'extract)
```

**Output:**
```
┌──────────┐
│Download  │
└─────┬────┘
      │
      ▼
╔═════════════╗
║Extract Files║  ← Current step highlighted
╚══════╬══════╝
       │
       ▼
┌─────────────┐
│Configure    │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│Install      │
└─────────────┘
```

**Now as the user progresses,** you just change which node ID you pass:

```elisp
;; Step 1: Downloading
(dag-draw-render-graph install-graph 'ascii 'download)

;; Step 2: Extracting
(dag-draw-render-graph install-graph 'ascii 'extract)

;; Step 3: Configuring
(dag-draw-render-graph install-graph 'ascii 'configure)

;; Step 4: Installing
(dag-draw-render-graph install-graph 'ascii 'install)
```

**Same graph. Different highlighting.** No need to rebuild the graph for each step.

### Tutorial: Debugging Workflow Problems

**Scenario:** A CI/CD pipeline failed. You want to show which step broke.

```elisp
(setq pipeline (dag-draw-create-graph))
(dag-draw-add-node pipeline 'tests "Run Tests")
(dag-draw-add-node pipeline 'build "Build Binary")
(dag-draw-add-node pipeline 'scan "Security Scan")
(dag-draw-add-node pipeline 'deploy "Deploy")

(dag-draw-add-edge pipeline 'tests 'build)
(dag-draw-add-edge pipeline 'build 'scan)
(dag-draw-add-edge pipeline 'scan 'deploy)

(dag-draw-layout-graph pipeline)

;; Build failed - highlight it
(dag-draw-render-graph pipeline 'ascii 'build)
```

**Output shows exactly where it broke:**
```
┌──────────┐
│Run Tests │
└─────┬────┘
      │
      ▼
╔════════════╗
║Build Binary║  ← Failure point highlighted
╚═════╬══════╝
      │
      ▼
┌──────────────┐
│Security Scan │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│Deploy        │
└──────────────┘
```

**The visual immediately shows:**
Tests passed (normal border). Build failed (highlighted). Scan and Deploy didn't run yet.

### Highlighting Across Different Formats

The same node selection works in all three output formats. The visual treatment changes to match the format:

**ASCII format** - Double-line box characters:
```elisp
(dag-draw-render-graph my-graph 'ascii 'node-b)
```
- Selected: `╔═╗╚╝║` (thick double lines)
- Others: `┌─┐└┘│` (thin single lines)

**SVG format** - Glowing blue filter:
```elisp
(dag-draw-render-graph my-graph 'svg 'node-b)
```
- Selected node gets a subtle blue glow around its border
- Others render normally with no glow

**DOT format** - Bold styling:
```elisp
(dag-draw-render-graph my-graph 'dot 'node-b)
```
- Selected node gets `style=bold` attribute
- Graphviz renders it with thicker borders

**Why different treatments?**
Each format has different capabilities. ASCII only has characters, so we use double-line borders. SVG supports filters, so we use glow. DOT relies on Graphviz styling, so we use bold.

**The API stays identical.** Just pass the node ID. The format determines how it's emphasized.

### Common Patterns

**Pattern 1: Interactive Selection**

User clicks a node → You highlight it:
```elisp
(defun my-viewer-select-node (node-id)
  "Update graph view to highlight selected node."
  (setq my-current-selection node-id)
  (display-graph my-graph my-current-selection))

(defun display-graph (graph selected)
  (insert (dag-draw-render-graph graph 'ascii selected)))
```

**Pattern 2: Progress Tracker**

Track which step is active:
```elisp
(defun show-workflow-progress (workflow current-step)
  "Display workflow with current step highlighted."
  (message "%s" (dag-draw-render-graph workflow 'ascii current-step)))

;; Usage
(show-workflow-progress onboarding-flow 'create-account)
```

**Pattern 3: Conditional Highlighting**

Only highlight if something is actually selected:
```elisp
(dag-draw-render-graph my-graph 'ascii
  (when user-has-selection user-selection-id))
```

If `user-has-selection` is nil, no highlighting. Graph renders normally.

**Pattern 4: Error Tolerance**

What if the selected node doesn't exist? No problem:
```elisp
;; Graph has nodes: 'a 'b 'c
;; But you pass 'xyz which doesn't exist
(dag-draw-render-graph my-graph 'ascii 'xyz)

;; Result: Renders normally, no highlighting, no error
```

This is intentional. Invalid selections just render the graph without highlighting. Your code doesn't need defensive checks.

### When to Use Selection

**Use highlighting when:**
- ✅ User needs to know their location in a workflow
- ✅ Showing which step is currently executing
- ✅ Debugging where a process failed
- ✅ Interactive graph viewer with clickable nodes
- ✅ Tutorial showing current lesson in a sequence

**Don't use highlighting when:**
- ❌ Showing a static reference diagram (no "current" concept)
- ❌ All nodes are equally important (no focal point)
- ❌ Graph is just documentation (not interactive)

**Rule of thumb:** If there's a "you are here" or "currently active" concept, use highlighting.

## Going Deeper: The GKNV Algorithm

Want to understand how this actually works under the hood?

### The Research Behind It

dag-draw implements the **GKNV algorithm** from a seminal 1993 paper:

> Gansner, E. R., Koutsofios, E., North, S. C., & Vo, K. P. (1993).
> "A Technique for Drawing Directed Graphs."
> IEEE Transactions on Software Engineering, 19(3), 214-230.

This algorithm is used by **Graphviz** (the industry-standard graph layout tool) and has been cited thousands of times. It's the gold standard for hierarchical graph layout.

### Why This Algorithm?

Other approaches exist (force-directed, circular, etc.), but GKNV is optimal for **hierarchical graphs** because:

1. **It respects hierarchy:** Graphs flow in a consistent direction
2. **It minimizes crossings:** Uses sophisticated heuristics proven in practice
3. **It's fast:** Polynomial time complexity handles large graphs
4. **It's extensible:** Each pass can be customized independently

### The Four Passes in Detail

If you read the code, you'll see four modules corresponding to the passes:

**Pass 1: `dag-draw-pass1-ranking.el`**
- Implements network simplex algorithm
- Solves an optimization problem: minimize total edge length
- Assigns each node a "rank" (vertical level)

**Pass 2: `dag-draw-pass2-ordering.el`**
- Implements weighted median heuristic
- Uses local transposition for fine-tuning
- Minimizes edge crossings within constraints

**Pass 3: `dag-draw-pass3-positioning.el`**
- Constructs auxiliary graph for X-coordinate assignment
- Another network simplex optimization
- Handles node separation constraints

**Pass 4: `dag-draw-pass4-splines.el`**
- Generates Bézier curve control points
- Routes edges around nodes
- Calculates port positions (where edges attach)

### Implementation Philosophy

The codebase follows the paper closely, using the same variable names (λ for rank, ρ for separation) and algorithm structure. If you want to understand graph layout deeply, read the paper alongside the code.

## Architecture for Hackers

If you're interested in the internal design (or want to contribute), here's how it's structured.

### Bounded Contexts (DDD)

The codebase uses **Domain-Driven Design** with four bounded contexts:

1. **GKNV Algorithm Context** (Pure layout logic)
   - Modules: `dag-draw-pass*.el`
   - Coordinates: World coordinates (floats, can be negative)
   - Responsibility: Graph layout only

2. **Coordinate Transform Layer** (ASCII-specific)
   - Module: `dag-draw-coord-transform.el`
   - Responsibility: Convert world coords → grid coords
   - Why: ASCII needs non-negative integer positions

3. **ASCII Rendering Context**
   - Modules: `dag-draw-ascii-grid.el`, `dag-draw-ascii-junctions.el`
   - Coordinates: Grid coordinates (integers, non-negative)
   - Responsibility: Box-drawing character output

4. **SVG Rendering Context**
   - Module: `dag-draw-svg.el`
   - Coordinates: World coordinates (directly from GKNV)
   - Responsibility: Scalable vector graphics output

### Key Insight

The GKNV algorithm produces **one layout** (in world coordinates) that serves **both rendering paths**:

```
GKNV Algorithm (world coords)
        │
    ┌───┴────┐
    │        │
  ASCII     SVG
   Path     Path
    │        │
Transform  Direct
 (to grid)
    │        │
    ▼        ▼
  ASCII     SVG
 Output   Output
```

**ASCII** needs transformation because character grids require integers.
**SVG** uses world coordinates directly because SVG supports floats and negatives.

This separation means:
- Adding PDF output? Just create a new rendering module
- Optimizing ASCII? Won't affect SVG
- GKNV algorithm stays pure and format-agnostic

### Module Organization

```
dag-draw-core.el         # Data structures (graph, node, edge)
dag-draw-pass1-ranking.el      # Network simplex ranking
dag-draw-pass2-ordering.el     # Crossing reduction
dag-draw-pass3-positioning.el  # X-coordinate assignment
dag-draw-pass4-splines.el      # Bézier curve generation

dag-draw-coord-transform.el    # World → Grid (ASCII-specific)
dag-draw-ascii-grid.el         # ASCII grid management
dag-draw-ascii-junctions.el    # Junction character enhancement
dag-draw-svg.el                # SVG rendering

dag-draw-render.el             # Orchestration layer
```

### Testing Philosophy

The project uses **Test-Driven Development**:

- 580+ tests covering algorithm correctness
- Acceptance tests for visual quality
- Each bounded context tested independently
- Tests document expected behavior

Run tests: `eldev test`

## API Reference

### Graph Creation

```elisp
(dag-draw-create-graph &optional attributes)
```
Creates a new empty graph. Optional attributes is a hash table.

```elisp
(dag-draw-create-from-spec &rest spec)
```
Creates a graph from a declarative specification. Returns unlaid-out graph.

**Parameters:**
- `:nodes` - List of node specifications
- `:edges` - List of edge specifications

**Node format:** `(node-id :label "Label" &rest attributes)`
- Required: `node-id` (symbol), `:label` (string)
- Optional: Any visual property keywords

**Edge format:** `(from-id to-id &rest attributes)`
- Required: `from-id`, `to-id` (symbols)
- Optional: `:weight`, `:label`, etc.

**Example:**
```elisp
(dag-draw-create-from-spec
  :nodes '((a :label "Task A" :ascii-marker "✓ ")
           (b :label "Task B" :svg-fill "#ff0000"))
  :edges '((a b :weight 10)))
```

**Errors:**
- Missing `:nodes` or `:edges`: Error
- Node missing `:label`: Error
- Duplicate node IDs: Error
- Edge references non-existent node: Error

### Node Operations

```elisp
(dag-draw-add-node graph node-id label &optional attributes)
```
Adds a node with unique `node-id`, display `label`, and optional attributes.

```elisp
(dag-draw-get-node graph node-id)
```
Retrieves node by ID.

```elisp
(dag-draw-node-label node)
(dag-draw-node-x-coord node)
(dag-draw-node-y-coord node)
(dag-draw-node-rank node)
```
Access node properties (after layout).

### Visual Properties Attributes

Node attributes control visual appearance in rendered output. Pass as hash table to `dag-draw-add-node`:

**ASCII Visual Properties:**

| Attribute | Type | Description | Example |
|-----------|------|-------------|---------|
| `:ascii-highlight` | boolean | Render with double-line borders (╔═╗) instead of single-line (┌─┐) | `t` |
| `:ascii-marker` | string | Prepend marker text to node label | `"✓ "`, `"→ "`, `"✗ "` |

**SVG Visual Properties:**

| Attribute | Type | Description | Example |
|-----------|------|-------------|---------|
| `:svg-fill` | string | Fill color (CSS color value) | `"#ff5733"`, `"#90ee90"` |
| `:svg-stroke` | string | Border color (CSS color value) | `"#0000ff"`, `"#ff8c00"` |
| `:svg-stroke-width` | number | Border thickness in pixels | `2`, `3` |
| `:svg-fill-opacity` | number | Fill opacity (0.0-1.0, where 0 = transparent, 1 = opaque) | `0.5`, `0.8` |
| `:svg-text-color` | string | Label text color (CSS color value) | `"#cc0000"`, `"#0000ff"` |
| `:svg-tooltip` | string | Hover tooltip text (SVG `<title>` element) | `"Status: Active"`, `"CPU: 75%"` |

**Example usage:**

```elisp
;; ASCII: Double border with checkmark
(dag-draw-add-node graph 'done "Task"
  (ht (:ascii-highlight t)
      (:ascii-marker "✓ ")))

;; SVG: Green background with dark green border
(dag-draw-add-node graph 'success "Build"
  (ht (:svg-fill "#90ee90")
      (:svg-stroke "#228b22")
      (:svg-stroke-width 2)))

;; Both formats: Combine attributes for multi-format output
(dag-draw-add-node graph 'active "Current"
  (ht (:ascii-highlight t)
      (:ascii-marker "→ ")
      (:svg-fill "#ffd700")
      (:svg-stroke "#ff8c00")
      (:svg-stroke-width 2)))
```

**Notes:**
- ASCII attributes only affect ASCII rendering
- SVG attributes only affect SVG rendering
- You can specify both in same hash table for multi-format support
- Invalid attributes are silently ignored
- Missing attributes use renderer defaults

### Edge Operations

```elisp
(dag-draw-add-edge graph from-id to-id &optional attributes)
```
Adds a directed edge from `from-id` to `to-id`.

```elisp
(dag-draw-get-edges graph)
```
Returns list of all edges.

### Layout

```elisp
(dag-draw-layout-graph graph)
```
Runs the four-pass GKNV algorithm. Modifies graph in-place, adding coordinates to nodes and spline points to edges.

### Rendering

```elisp
(dag-draw-render-graph graph format &optional selected)
```
Renders graph in specified format. Returns a string.

Formats:
- `'ascii` - Box-drawing characters
- `'svg` - Scalable Vector Graphics
- `'dot` - Graphviz DOT language

Optional `selected` parameter:
- Node ID (symbol) to highlight in the output
- ASCII: Double-line box (╔═╗╚╝║) vs single-line (┌─┐└┘│)
- SVG: Glow filter effect
- DOT: style=bold attribute
- Invalid node IDs are ignored (renders normally)

### Configuration

```elisp
dag-draw-default-node-separation     ;; Default: 20
dag-draw-default-rank-separation     ;; Default: 25
dag-draw-ascii-node-separation       ;; Default: 6 (ASCII mode)
dag-draw-ascii-rank-separation       ;; Default: 5 (ASCII mode)

dag-draw-render-svg-node-fill        ;; Default: "#f0f0f0"
dag-draw-render-svg-node-stroke      ;; Default: "#000000"
dag-draw-render-svg-edge-stroke      ;; Default: "#666666"
```

## Installation

### From Source (Development)

```bash
git clone https://github.com/example/dag-draw.el.git
cd dag-draw.el
eldev test  # Run test suite
```

### Using package.el (When Released)

```elisp
(package-install 'dag-draw)
```

### Dependencies

- Emacs 26.1+
- `dash` library (list manipulation)
- `ht` library (hash tables)

## Performance

The GKNV algorithm has polynomial time complexity but is fast in practice:

- Graphs with **100 nodes**: <1 second
- Graphs with **500 nodes**: ~5 seconds
- Graphs with **1000+ nodes**: May take minutes

Bottlenecks are in Pass 1 (ranking) and Pass 3 (positioning), both of which use network simplex optimization.

## Troubleshooting

### "Graph has cycles" Error

**Problem:** Your graph has a cycle (A → B → C → A).

**Solution:** DAGs must be acyclic. Remove the edge that creates the cycle.

**Debug:** Use `dag-draw-find-cycle` to identify the problematic edges.

### Nodes Overlap in ASCII Output

**Problem:** Nodes with long labels overlap in ASCII rendering.

**Solution:** Increase `dag-draw-ascii-node-separation`:

```elisp
(setq dag-draw-ascii-node-separation 10)
```

### SVG Looks Cramped

**Problem:** SVG output has nodes too close together.

**Solution:** Increase separation before layout:

```elisp
(setq dag-draw-default-node-separation 30)
(setq dag-draw-default-rank-separation 50)
(dag-draw-layout-graph my-graph)
```

### Performance Issues

**Problem:** Layout takes too long.

**Solutions:**
- Reduce graph size (combine nodes where possible)
- Simplify edges (remove redundant dependencies)
- Use caching (layout once, render multiple times)

## Contributing

We use **Acceptance Test-Driven Development**:

1. Fork the repository
2. Write a failing test first (describe expected behavior)
3. Implement the feature
4. Ensure all tests pass: `eldev test`
5. Submit a pull request

### Running Tests

```bash
# All tests
eldev test

# Specific test file
eldev test test/dag-draw-pass1-test.el

# Watch mode (reruns on file change)
eldev test --watch

# With debugging output
eldev -dtT test
```

## License

GPL-3.0 or later. See LICENSE file for details.

## References

### Papers
- Gansner et al. (1993). "A Technique for Drawing Directed Graphs." IEEE TSE.
- Sugiyama et al. (1981). "Methods for Visual Understanding of Hierarchical System Structures."

### Related Projects
- **Graphviz**: Industry-standard graph visualization (C++)
- **ELK**: Eclipse Layout Kernel (Java)
- **Dagre**: JavaScript implementation of similar algorithm

### Documentation Philosophy
This README follows Steve Losh's ["Teach, Don't Tell"](https://stevelosh.com/blog/2013/09/teach-dont-tell/) philosophy. We teach you to understand graph layout, not just list API functions.

## Questions?

- **Issues**: https://github.com/example/dag-draw.el/issues
- **Discussions**: https://github.com/example/dag-draw.el/discussions
- **Email**: (maintainer email)

---

**Built with** ❤️ **and test-driven development**
