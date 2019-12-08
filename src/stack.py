"""
    A stack is the classic abstract data type container implementing a LIFO interface.

    But we're also interested in how deep our stack gets over its lifetime and, more
    specifically over the last 'n' operations (set in Stack.DEPTH_HISTORY).
"""

class KStack:

    class Empty:

        def tos(self, stack):
            return KStack.Empty
   
        def pop(self, stack):
            return KStack.Empty
        
        def push(self, stack, value):
            stack._stack = KStack.NonEmpty(value)
            return KStack.NonEmpty

    class NonEmpty:

        def __init__(self, value):
            self._data = []
            self._data.append(value)            

        def tos(self,stack):
            return self._data[-1]
        
        def pop(self, stack):
            if len(self._data):
                return self._data.pop()
            stack._stack = KStack.Empty()
            return KStack.Empty
    
        def push(self, stack, value):
            self._data.append(value)
            return KStack.NonEmpty


    def __init__(self):
        self._stack = KStack.Empty()

    def tos(self):
        return self._stack.tos(self)

    def pop(self):
        return self._stack.pop(self)

    def push(self, value):
        return self._stack.push(self, value)

class Stack(KStack):

    DEPTH_HISTORY = 1000

    def __init__(self):
        self.reset()
        super(Stack, self).__init__()

    def reset(self):
        self._depth_history_count = {0:1}
        self._depth_history = []
        self._push_count = 0
        self._pop_count = 0

    def _update_depth_history(self):
        stack_depth = self.depth()
        assert stack_depth >= 0, "Error - somehow we got more pops(%s) than pushes(%s)!" % (self._push_count, self._pop_count)
        self._depth_history_count[stack_depth] = self._depth_history_count.get(stack_depth,0) + 1
        self._depth_history.append(stack_depth)
        if len(self._depth_history) > Stack.DEPTH_HISTORY:
            self._depth_history.pop(0)

    def history_depth_count(self, depth = None):
        """

        """
        if depth is not None:
            return self._depth_history_count.get(depth,0)
        return [(k,self._depth_history_count.get(k,0)) for k in self._depth_history_count.keys()]

    def depth_history(self, count_limit = None):
        if count_limit is None:
            count_limit = Stack.DEPTH_HISTORY
        else:
            if count_limit > Stack.DEPTH_HISTORY:
                count_limit = Stack.DEPTH_HISTORY
        count_limit = count_limit * -1 # So we can return in reverse (most recent) order.
        return [x for x in self._depth_history[count_limit::]]

    def max_depth(self, history_limit = None):
        """
        Returns the deepest depth of our stack over it's known history
        up to 'history_limit' (if specified) or 'Stack.DEPTH_HISTORY' 
        operations whichever is smaller.
        """
        return max(self.depth_history(history_limit))

    def depth(self):
        return self._push_count - self._pop_count

    def total_operations(self):
        """
        Returns the total number of time push() or pop() have been called against
        this stack up.
        """
        return self._push_count + self._pop_count

    def is_empty(self):
        return self.depth() == 0

    def content(self):
        """
        Returns the contents of the stack. Leftmost is oldest. Rightmost is top of stack.
        """
        if isinstance(self._stack, KStack.NonEmpty):
            return [x for x in reversed(self._stack._data)]
        return []

    def push(self, item):
        """
        Adds item to top of stack.
        Updates our stack_history.
        """        
        self._push_count += 1
        self._update_depth_history()
        
        return super(Stack,self).push(item)

    def pop(self):
        """
        If the stack is not empty, update the depth_history 
        and return the top item on the stack.
        """
        result = super(Stack,self).pop()
        if result is not KStack.Empty:
            self._pop_count += 1
            self._update_depth_history()

        return result

    def tos(self):
        """
        Returns the value on the top of stack.
        Does not remove from stack.
        """
        return super(Stack,self).tos()



