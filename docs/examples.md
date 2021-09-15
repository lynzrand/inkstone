# Inkstone Examples

## A game

```ruby
use inkstone::game

let env_proto = {
	__proto: game_env_proto,
	visit: |self| do
		self.visited = true
		self.visit_cnt = (self.visit_cnt + 1)
	end
}

def new_env() do
	{
		__proto: env_proto,
		visited: false,
		visit_cnt: 0
	}
end

def game_start(implicit env) do
	env "An old house appeared in front of you."
	env "It looked dark and scary."
	
	env.choice "Are you visiting it?" [
		("Nope, it looked too scary.", || tail ending_0),
		("Yep, worth a try.", || tail visit)
	]
end

def visit(implicit env) do
	env "You opened the door."
	env "It is filled withthe smell of old wood, but nothing interesting happened."
	
	(env.visit)
	
	env.choice "Visit again?" [
		("Yes!", || tail visit),
		("Not really.", || if env.visit_cnt < 5; tail ending_1; else tail ending_2; end)
	]
end

def ending_0(implicit env) do
	env "You walked away."
end

def ending_1(implicit env) do
	env "A ghost appeared at the door."
	env "You disturbed my house! Now I won't let you go." speaker: "Ghost"
	env "The ghost pulled your soul out of your body."
end

def ending_2(implicit env) do
	env "The house suddenly bursted into flames."
	env "You're scared the shit out of your pants, and ran at your maximum speed out of it."
	
	std::task::call_cc |cont| do
		std::task::yield_as "user::game-main" cont
	end
end

let env = (new_env)
game_start env
```
