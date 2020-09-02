"""
    A stack is the classic abstract data type container implementing a LIFO interface.

    But we're also interested in how deep our stack gets over its lifetime and, more
    specifically over the last 'n' operations (set in Stack.DEPTH_HISTORY).
"""
from typing import Sequence, Any

class KStack:

    class Empty:

        def tos(self, stack):
            return KStack.Empty

        def pop(self, stack):
            return KStack.Empty

        def push(self, stack, value):
            stack._stack = KStack.NonEmpty(value)
            return KStack.NonEmpty

        def copy(self):
            return KStack.Empty()

    class NonEmpty:

        def __init__(self, value):
            self._data = []
            self._data.append(value)

        def tos(self, stack):
            if len(self._data):
                return self._data[-1]

            # Stack is empty if we get this far.
            stack._stack = KStack.Empty()
            return KStack.Empty

        def pop(self, stack):
            ## TODO : This is a bit dangerous because we won't catch a
            ##        stack underflow.
            if len(self._data):
                return self._data.pop()

            # Stack is empty if we get this far.
            stack._stack = KStack.Empty()
            return KStack.Empty

        def push(self, stack, value):
            self._data.append(value)
            return KStack.NonEmpty

        def copy(self):
            result = KStack.NonEmpty(None)
            result._data = self._data.copy()
            return result


    def __init__(self):
        self._stack = KStack.Empty()

    def tos(self):
        return self._stack.tos(self)

    def pop(self):
        return self._stack.pop(self)

    def push(self, value):
        return self._stack.push(self, value)

    def copy(self):
        result = KStack()
        result._stack = self._stack.copy()
        return result

class Stack(KStack):

    DEPTH_HISTORY = 1000

    def __str__(self):
        result = "\nstack:\n"
        if self.depth() == 0:
            result += "\t(stack empty)"
        else:
            for n in reversed(self.contents()):
                result +='\t%s\n'%str(n)        
        return result


    def __repr__(self):
        return self.__str__()

    def __init__(self, in_seq: Sequence[Any] = None):
        self.reset()
        super(Stack, self).__init__()
        if in_seq is not None:
            for n in in_seq:
                self.push(n)

    def reset(self):
        self._depth_history_count = {0:1}
        self._depth_history = []
        self._push_count = 0
        self._pop_count = 0
        self._max_depth = 0

    def _update_depth_history(self):
        stack_depth = self.depth()
        if stack_depth > self._max_depth: self._max_depth = stack_depth
        assert stack_depth >= 0, "Error - somehow we got more pops(%s) than pushes(%s)!" % (self._push_count, self._pop_count)
        self._depth_history_count[stack_depth] = self._depth_history_count.get(stack_depth,0) + 1
        self._depth_history.append(stack_depth)
        if len(self._depth_history) > Stack.DEPTH_HISTORY:
            self._depth_history.pop(0)

    def history_depth_count(self, depth = None):
        if depth is not None:
            return self._depth_history_count.get(depth,0)
        return [(k,self._depth_history_count.get(k,0)) for k in self._depth_history_count.keys()]

    def depth_history(self, count_limit = None):
        """
        Return the depth history of the stack.
        """
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
        return self._max_depth

    def depth(self):
        """
        Returns the stack depth.
        """
        return self._push_count - self._pop_count

    def total_operations(self):
        """
        Returns the total number of time push() or pop() have been called against
        this stack up.
        """
        return self._push_count + self._pop_count

    def is_empty(self):
        """
        Returns if the stack is empty.
        """
        return self.depth() == 0

    def contents(self, last: int = 0):
        """
        Returns the contents of the stack. Leftmost is oldest. Rightmost is top of stack.
        If last is not 0 then return the last 'n items off the stack.        
        """
        if isinstance(self._stack, KStack.NonEmpty):
            if last == 0:
                return [x for x in self._stack._data]
            else:
                if last > self.depth():
                    raise Exception("ERROR: Stack underflow. Request for last %i items from a stack with only a depth of %i!" % (last, self.depth()))
                return self._stack._data[last*-1:]                                    
        return []

    def __len__(self):
        return self.depth()

    def push(self, item):
        """
        Adds item to top of stack and updates the stack_history.
        """
        self._push_count += 1
        self._update_depth_history()

        return super(Stack,self).push(item)

    def pop(self):
        """
        If the stack is not empty, update the depth_history and return the
        top item on the stack.
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

    def copy(self):
        result = Stack()
        result._stack = self._stack.copy()
        result._depth_history_count = self._depth_history_count
        result._depth_history = self._depth_history
        result._push_count = self._push_count
        result._pop_count = self._pop_count

        return result

    def __eq__(self, s : object) -> bool:
        if not isinstance(s, Stack):
            return NotImplemented

        if len(self) != len(s): return False
        return all([(a==b) for (a,b) in zip(self.contents(), s.contents())])
