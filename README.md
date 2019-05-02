# ExpressionEvaluator

Our program can transfer mathematical expression to postfix, infix or prefix. It uses synthatical tree. We can also substitute certain variables or evaluate the expression.

First we need to preprocess the input so we do not have any unnecessary symbols in expression like leading blanks and etc. To this purpose, we have a function tokenize which split a string into operands and operators and store them into list.
Secondly we need to convert our expression into postfix. We have implemented Shunting yard algorithm, which was created by Edsger Dijsktra. Main idea of algorithm is described in comments in source code.
After the expression is converted into postfix notation we can create a synthatical tree. We have two function: parse and add. Parse as the name suggests it just process the input. Function add has two input arguments: expression in postfix and stack of trees (vertices). It is a similar approach as in Dijkstra algorithm. If we have a operator, we create a vertice, add two childs from stack and push whole new tree into stack. Otherwise we push operands into stack.
From synthatical tree we can acquire all three type of notations by traversing the tree in preorder, inorder or postorder.

