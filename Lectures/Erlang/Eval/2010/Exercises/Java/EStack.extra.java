
  public EStack<Item> rm_bot() { return this; }

  protected EStack<Item> rm_bot_aux (final Item item) { 
    return this; }

  public Stack<Item> shuffle(final Stack<Item> stack) {
    return stack; }

  public EStack<Item> rep_fst() {
    return this; }

  protected Stack<Item> rep_fst_aux(final Item item) {
    return this; }

  public Stack<Item> merge(final Stack<Item> stack) {
    return stack; }

  protected NStack<Item> merge_aux(final Item item,
                                   final Stack<Item> stack,
                                   final NStack<Item> orig) {
    return orig; }

  public Ext<Item> to_BST() { return new Ext<Item>(); }
