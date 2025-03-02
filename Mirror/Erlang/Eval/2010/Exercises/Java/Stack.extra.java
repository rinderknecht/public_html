  public             Stack<Item>  rev() { return rev_cat(new EStack<Item>()); }
  public    abstract Stack<Item>  rev_cat(final Stack<Item> stack);
  public    abstract Stack<Item>  rm_bot();
  protected abstract Stack<Item>  rm_bot_aux(final Item item);
  public    abstract Stack<Item>  rm_fst(final Item item);
  public    abstract Stack<Item>  rm_lst(final Item item);
  protected abstract Stack<Item>  rm_lst_aux(final Item item, 
                                             final Stack<Item> orig);
  public    abstract Stack<Item>  shuffle(final Stack<Item> stack);
  public    abstract Stack<Item>  isort();
  protected abstract NStack<Item> insert(final Item item);
  public    abstract Stack<Item>  rep_fst();
  protected abstract Stack<Item>  rep_fst_aux(final Item item);
  public    abstract Stack<Item>  merge(final Stack<Item> stack);
  protected abstract Stack<Item>  merge_aux(final Item item,
                                            final Stack<Item> stack,
                                            final NStack<Item> orig);
  public    abstract BST<Item>    to_BST();
