# Inkstone Examples

## A game

```ruby
use inkstone::game::*
use inkstone_unity_adapter::UnityAdapter

pub let env = Env::new (UnityAdapter::new ())
let sylvie = env.character "Sylvie" text_color: 0xc8ff88
let player = env.character "Me" text_color: 0xffffff

pub def game_start = begin
  start ()
end

def start = begin
  env.set background: "meadows"

  env "After a short while, we reach the meadows just outside the neighborhood where we both live."
  env "It's a scenic view I've grown used to. Autumn is especially beautiful here."
  env "When we were children, we played in these meadows a lot, so they're full of memories."

  player "Hey... Umm..."

  # This line
  sylvie.set image_overlay: "smile"

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

  sylvie.set image_overlay: "surprised"

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
