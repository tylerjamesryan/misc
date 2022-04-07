
# This is an R script. You should see the console in one of the other panes
# in RStudio. When you execute (run) code, the console will display the code
# that you executed and possibly the result of executing it. You can execute
# code in a script several ways. If you place your cursor on a line and click
# the "Run" button above, it will execute that line. There are also hotkeys for
# running the line. On my mac, I can press command+Enter to run the line. You
# can highlight multiple lines to run them sequentially from top to bottom. You
# can also highlight something and run only the selection you highlighted rather
# than the entire line. Lines that start with a pound sign / hashtag (#) are
# called comments. Comments are not executed. Feel free to write your own
# comments to take notes. 

# Run the following lines and check out the console.

145
405727

# Basic math operations on elements ####

# R is a calculator. When you run these lines, the console prints both the code
# you ran and the output or result of each line. Give it a try.

5 + 8
145 / 4538
134.54 * 23

# R follows the order of operations (remember good ole PEMDAS?)

1 + 4 * 8
(1 + 4) * 8

# R will evaluate anything in a set of parentheses first. If you forget to
# "close" your parentheses, or have one that doesn't have a matching partner, R
# will do one of two things. If your forgot the left or open parenthesis, R will
# give you an error. RStudio will also "flag" that something's wrong on the
# left-hand side.

# 1 + 4) * 8

# If you forget the right or closing parenthesis, R will print a plus sign to
# the console, indicating that it is waiting for a closing parenthesis. It will
# not evaluate the code until it gets a closing parenthesis or the Esc button is
# pushed.

# (1 + 4 * 8

# You might encounter scientific notation in R when numbers get really really
# big or really really small

100000000000000 # run this. R prints the number in scientific notation 1e+14
.00000000000001 # 1e-14

# If its not that small or not that large, R will print the number
1e+12
1000000000000
1e-4
1e+4

# Objects ####  

# Instead of printing the results to the console, you can save the results as an
# object. You can name your object just about anything you want as long as it
# starts with a letter. It can also contain periods and underscores. To assign
# some value to an object, use the assignment operator `<-`, which is just the
# less-than sign and minus sign, or you can use the equal sign `=`.

a = 4

# When you create an object like this, it saves it to the "Global Environment".
# You can see of the objects in your Global Environment by clicking on the
# "Environment" panel tab.

# You can execute your object to retrieve its content.

a

# More importantly, you can use the value of your object with other code or
# other objects

a + 10
a^9 # shift 6
a + a
banana = 1382
a / banana

# You can overwrite your object with a new value

a = 5

# If you run the lines with your `a` object above again, it should show
# different values because it is using the new value. 

# If you try and run code that you have not assigned a value to, it will give
# you an error.

ohio
ohio = 1 + 56 + 43

# Can you guess what the value of `a` will be before you run the following
# lines?

a <- a + 25 # 30
a <- a + 15 # 30 + 15
a

# Functions ####

# R uses functions. Functions are just a chunk of code that takes some input
# and produces some output. Just like our `a` object above, functions are
# objects as well. The input a function takes is referred to as an argument. For
# example, the `sqrt()` function takes a single argument that it calls `x` as an
# input and then performs the squre-root operation on it. When we use a
# function, we refer to that as "calling" the function.

sqrt(x = 25)

# A function has its own environment inside itself that is different from the
# Global Environment. You can think of this as temporarily assigning the value
# 25 to the argument object `x` inside the `sqrt()` function, and then taking
# the square-root of x. This assignment must use the equals sign, NOT the
# assignment operator `<-` when inside a function. 

# You can even put functions inside functions.

sqrt(x = sqrt(x = 25))

# R evaluates the innermost function first and then passes that value to the
# next function.

# For some functions, you don't need to write out the argument name. You can
# just give the function a value. If a function has multiple arguments, R
# assigns values to arguments in functions in the order that you pass them.

output = sqrt(25)

# This is fine for simple functions like this. When we use more complicated
# functions, I will encourage you to explicitly write the argument names.

# Note that we have not assigned any value to `x` in the Global Environment, it
# doesn't exist there. Furthermore, it only exists in the `sqrt()` function
# while that function is running. Once it is finished executing, the environment
# inside `sqrt()` is erased.

# We can even write our own function and save it to the Global Environment. We
# can name it anything we want, as long as it follows the same naming rules as
# any other object.

addition <- function(number1, number2) {
  result <- number1 + number2
  
  return(result)
}

addition(number1 = 100, number2 = 23)

# This function has two arguments, each asking for a number. It then adds those
# numbers and assigns the result to the object `result` inside the `addition()`
# environment. Finally, the `return()` function tells R to return something, in
# this case our result object. Try it out.

addition(56, 24) # same as 56 + 24

# Run just the function name, like you would any other object, and see what its
# value is.

addition

# Classes and Types ####

# Most objects in R have an attribute called "class". There are several
# different classes in R. Here are some important ones.

# The numbers we've been using above are of class "numeric". Go figure. You can
# check the class of something using the `class()` function. Class takes a
# single argument, `x`, and returns its class.

50

class(x = 50)

octopus <- 45.24
class(x = octopus)

# Look at the class of our `addition()` function.

class(addition)

# Character Strings ####

# Another important class of objects are "character" objects. Character objects
# can be executed like numbers, but must have quotation marks around them. These
# are also often called "string" objects.

"apple"
"4527"
"%$@%&*^$##@"
"If Tommy has 15 Tide Pods and eats 12 of them, how long does he have to live?"
"" 

class("what class is this?")
i_dont_know <- "you tell me"
class(i_dont_know)

class(class)
class(class(i_dont_know))

# This may seem like dull programming stuff, but it's important to understand.
# Some functions can only take certain classes of objects for certain arguments.
# For example, what happens if you try to take the square-root of a character
# object?

sqrt(x = "this isn't going to work")

# R throws an error. The error output in the console tells you what the problem
# is. `sqrt()` requires a numeric argument and we passed it a non-numeric one.
# You WILL encounter lots of errors using R. This is one of the most common and
# can happen in a lot of different ways.

# Some objects can be coerced from one class to another. The `as.numeric()`
# function transforms objects into numeric objects.

class("145.5")
as.numeric("145.5")
class(as.numeric("145.5"))

# Some objects obviously can't.
as.numeric("october")
class(as.numeric("october"))

# Logical class ####

# One last really important class is the "logical" class. In R, the words TRUE
# and FALSE are reserved when they are in all-caps. You cannot assign anything
# to them. 

TRUE
FALSE
class(TRUE)

# The capital letters T and F are also shorthand versions of TRUE and
# FALSE, but T and F can be assigned other values.

T
F
class(F)

# Logical objects can be coerced to numeric objects. TRUE gets coerced to 1 and
# FALSE gets coerced to 0. 

as.numeric(TRUE)
as.numeric(FALSE)

# This is often done automatically with a lot of functions that take numeric
# arguments.

TRUE + TRUE
FALSE - TRUE
addition(number1 = TRUE, number2 = TRUE)

# Numeric objects can also be coerced into logicals

as.logical(1)
as.logical(0)

# A TRUE value can be "flipped" to a FALSE value by putting an exclamation point
# in front of it, indicating "NOT TRUE". Same thing with FALSE.

!TRUE
!FALSE

# Logicals are most often used to check if two objects are equivalent, or if one
# object is greater than or less than another.

# Equivalence can be tested two equals signs `==`

5 == 5
4.5 == 10
a == a
octopus == 45.24
5^2 == 25
"cashew" == "cashew"
"cashew" == "Gremlins is a xmas movie, fight me"

# Non-equivalence can be tested with an exclamation point and an equals sign
# `!=`

23 != "twenty-three"
a != a

# The less-than and greater-than signs can also be used for numeric objects

23 > -80
23 < 0
23 < 23
23 <= 23

# Multiple logical test can be combined with the "&" (and) and "|" (or)
# operators. The & operator tests of two comparisons are TRUE. The | operator
# tests if at least one comparison is TRUE.

23 > 21 & "Elvis" == "Elvis"
8 == 2 & "Elvis" == "Elvis"
FALSE & "Elvis" == "Elvis"

45 == 44 + 7 | 1 + 1 == 2



