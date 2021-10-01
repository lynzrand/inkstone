# Inkstone Examples

## A game

```ruby
use inkstone::game

let env_proto = {
    __proto: game_env_proto,
    visit: \self -> begin
        self.visited = true
        self.visit_cnt = (self.visit_cnt + 1)
    end
}

def new_env = begin
    {
        __proto: env_proto,
        visited: false,
        visit_cnt: 0
    }
end

def game_start env = begin
    env "An old house appeared in front of you."
    env "It looked dark and scary."
    
    env.choice "Are you visiting it?" [
        ("Nope, it looked too scary.", \-> ending_0 env),
        ("Yep, worth a try.", \-> visit env)
    ]
end

def visit env = begin
    env "You opened the beginor."
    env "It is filled withthe smell of old wood, but nothing interesting happened."
    
    env.visit ()
    
    env.choice "Visit again?" [
        ("Yes!", \-> visit env),
        ("Not really.", \-> if env.visit_cnt < 5; ending_1 env; else ending_2 env; end)
    ]
end

def ending_0 env = begin
    env "You walked away."
end

def ending_1 env = begin
    env "A ghost appeared at the beginor."
    env "You disturbed my house! Now I won't let you go." speaker: "Ghost"
    env "The ghost pulled your soul out of your body."
end

def ending_2 env = begin
    env "The house suddenly bursted into flames."
    env "You're scared the shit out of your pants, and ran at your maximum speed out of it."
    
    std::task::yield
end

let env = new_env ()
game_start env
```
