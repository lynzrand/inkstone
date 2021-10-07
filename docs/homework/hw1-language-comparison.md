
## 语法对比

### RenPy

```rpy
define s = Character('Sylvie', color="#c8ffc8")
define m = Character('Me', color="#c8c8ff")

label start:

    scene bg meadow

    "After a short while, we reach the meadows just outside the neighborhood where we both live."

    "It's a scenic view I've grown used to. Autumn is especially beautiful here."

    "When we were children, we played in these meadows a lot, so they're full of memories."

    m "Hey... Umm..."

    show sylvie green smile

    "She turns to me and smiles. She looks so welcoming that I feel my nervousness melt away."

    "I'll ask her...!"

    m "Ummm... Will you..."

    m "Will you be my artist for a visual novel?"

    show sylvie green surprised

    "Silence."
```

RenPy 的语法是三者中最干净的一个，但是逻辑复杂度不够。

### Ink

```ink
=== back_in_london ===

We arrived into London at 9.45pm exactly.

*	"There is not a moment to lose!"[] I declared.
	-> hurry_outside

*	"Monsieur, let us savour this moment!"[] I declared.
	My master clouted me firmly around the head and dragged me out of the door.
	-> dragged_outside

*	[We hurried home] -> hurry_outside


=== hurry_outside ===
We hurried home to Savile Row -> as_fast_as_we_could


=== dragged_outside ===
He insisted that we hurried home to Savile Row
-> as_fast_as_we_could


=== as_fast_as_we_could ===
<> as fast as we could.
```

Ink 的语法看起来十分像写剧本，但是看起来不怎么干净，乱七八糟的符号比较多。

### KAG

```kag
[image storage="bg0" page=fore layer=base]
[wait time=200]
*start|Start
[cm]
Hello. The background layer is switched. [l][r]
[backlay]
[image storage="bg1" layer=base page=back]
[trans method=crossfade time=1500]
[wt]
Did it change?
```

KAG 完全就是 tag 打架了，这还只是 demo。我看过实际程序，一眼看过去根本看不清。

### Inkstone（提案稿）

> 恕我无耻地使用了 RenPy 的 demo 做例子

```ruby
use inkstone::game::*
use inkstone_unity_adapter::UnityAdapter

pub let env = Env::new (UnityAdapter::new ())
let sylvie = env.character "Sylvie" text_color:0xc8ff88
let player = env.character "Me" text_color:0xffffff

pub def game_start = begin
  start ()
end

def start = begin
  env.set :background "meadows"

  env "After a short while, we reach the meadows just outside the neighborhood where we both live."
  env "It's a scenic view I've grown used to. Autumn is especially beautiful here."
  env "When we were children, we played in these meadows a lot, so they're full of memories."

  player "Hey... Umm..."

  # This line
  sylvie.set :image_overlay "smile"

  env "She turns to me and smiles. She looks so welcoming that I feel my nervousness melt away."
  env "I'll ask her...!"

  env.choice [
    ("Will you be my artist for a visual novel?", \-> ask_sylvie (\-> run_away :asked)),
    ("Nothing at all!", \-> begin player "Noth... Nothing at all!"; run_away :not_asked; end)
  ] as: player
end

def ask_sylvie cont = begin
  player "Ummm... Will you..."
  player "Will you be my artist for a visual novel?"

  sylvie.set :image_overlay "surprised"

  env "Silence."

  cont ()
end

def run_away asked = begin
  sylvie.show false
  env "I ran away with my face burning red."

  if asked == :asked
    env "I've never requested a girl like that."
    env "And just then I heard Sylvie shouting from behind:"
    sylvie "I'll try as long as you don't mind!"
  end
end
```
