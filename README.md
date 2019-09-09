# Haskell Pong

I'm new to Haskell and made this Pong game as a way to learn Haskell. I've made the repository public because I wish for people to criticise my work and offer improvements. I'd like to see what a more experienced Haskell programmer would say about this and what improvements/changes they would suggest.

# The Game

For anyone interested in just playing the game, the two paddles are controlled by the same keyboard. To control the red paddle on the left, use the W and S keys. To control the green paddle on the right, use the UP and DOWN keys.

Hitting the ball on the edges of a paddle cause the ball to move away diagonally, while hitting the ball in the center of a paddle causes the ball to move horizontally.

After each successful hit the ball speed increases, and so does your paddle speed but to a lesser extent. When the ball slips past a paddle and hits the wall, the opposite team gets a score point and the ball's speed is reset.

# Requirements/Setup

To run this you need to have SDL installed.

You need to link to SDL when you want to run this with ghci: 

```
#!haskell

$ ghci main.hs -L/usr/lib/x86_64-linux-gnu/ -lSDL
```


# Contribution guidelines

If you see something that could be done better, feel free to fork it and make your changes. But please inform me so I can see what you've done and learn from it. Lots of descriptive comments would also be appreciated.

I'm especially interested in seeing how the code could be improved to adhere better to functional programming principles. If someone wants to change it to use Netwire I'd also be very eager to see this.

# Author

Joshua Woods (jpw1991)
