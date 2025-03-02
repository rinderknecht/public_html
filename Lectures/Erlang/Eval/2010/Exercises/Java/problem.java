  public static <Item> Stack<Item> fromArray
    (final Item[] items, final int len, final Stack<Item> stack) {
    return len == 0 ?
           stack
         : fromArray(items,len-1,stack.push(items[len]));
  }

  public NStack(final Item[] items) throws EmptyArray {
    if (items.length == 0) throw new EmptyArray();
    head = items[0];
    tail = fromArray(items,items.length-1,new EStack<Item>());
  }
