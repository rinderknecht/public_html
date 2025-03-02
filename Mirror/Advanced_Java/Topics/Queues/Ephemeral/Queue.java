public class Queue<Key> {
    protected Cell<Key> rear, front;

    public Queue () { rear = null; front = null; }
 
    public void enqueue (final Key k) {
      Cell<Key> last = new Cell<Key>(k,null);
      if (front == null) {
        rear = last; front = rear;
      }
      else { rear.next = last; rear = last; }
    }

    public boolean isEmpty () { return rear == null; }

    public void print () {
      if (!isEmpty()) { front.print(); System.out.println(); }
    }

    public void cat (final Queue<Key> q) {
      if (isEmpty()) { rear = q.rear; front = q.front; }
      else { rear.next = q.front; rear = q.rear; 
             q.rear = null; q.front = null; }
    }

}
