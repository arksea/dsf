using java.io;
using java.util;
using java.util.concurrent;
using java.util.concurrent.locks;

namespace org.apache.commons.pool2.impl
{

    internal class LinkedBlockingDeque<E> : java.util.AbstractQueue, java.util.Deque
	{
		private const long serialVersionUID = -387911632671998426L;

		/// <summary>
		/// Doubly-linked list node class </summary>
		private sealed class Node<T>
		{

			internal T item;
			internal Node<T> prev;
			internal Node<T> next;

			internal Node(T x, Node<T> p, Node<T> n)
			{
				item = x;
				prev = p;
				next = n;
			}
		}

		private Node<E> first;

		private Node<E> last;

		/// <summary>
		/// Number of items in the deque </summary>
		private int count;

		/// <summary>
		/// Maximum number of items in the deque </summary>
		private readonly int capacity;

		/// <summary>
		/// Main lock guarding all access </summary>
		private readonly InterruptibleReentrantLock _lock = new InterruptibleReentrantLock();

		/// <summary>
		/// Condition for waiting takes </summary>
		private Condition notEmpty;
		/// <summary>
		/// Condition for waiting puts </summary>
		private Condition notFull;

		public LinkedBlockingDeque() : this(int.MaxValue)
		{
		}

		public LinkedBlockingDeque(int capacity)
		{
			if (capacity <= 0)
			{
				throw new System.ArgumentException();
			}
            notEmpty = _lock.newCondition();
            notFull = _lock.newCondition();
			this.capacity = capacity;
		}

		public LinkedBlockingDeque(java.util.Collection c) : this(int.MaxValue)
		{
			_lock.@lock(); // Never contended, but necessary for visibility
			try
			{
                java.util.Iterator it = c.iterator();
                while(it.hasNext())
				{
                    E e = (E)it.next();
					if (e == null)
					{
                        throw new java.lang.NullPointerException();
					}
					if (!linkLast(e))
					{
						throw new java.lang.IllegalStateException("Deque full");
					}
				}
			}
			finally
			{
				_lock.unlock();
			}
		}

		private bool linkFirst(E e)
		{
			// assert lock.isHeldByCurrentThread();
			if (count >= capacity)
			{
				return false;
			}
			Node<E> f = first;
			Node<E> x = new Node<E>(e, null, f);
			first = x;
			if (last == null)
			{
				last = x;
			}
			else
			{
				f.prev = x;
			}
			++count;
			notEmpty.signal();
			return true;
		}

		private bool linkLast(E e)
		{
			// assert lock.isHeldByCurrentThread();
			if (count >= capacity)
			{
				return false;
			}
			Node<E> l = last;
			Node<E> x = new Node<E>(e, l, null);
			last = x;
			if (first == null)
			{
				first = x;
			}
			else
			{
				l.next = x;
			}
			++count;
			notEmpty.signal();
			return true;
		}

		private E unlinkFirst()
		{
			// assert lock.isHeldByCurrentThread();
			Node<E> f = first;
			if (f == null)
			{
                return default(E);
			}
			Node<E> n = f.next;
			E item = f.item;
			f.item = default(E);
			f.next = f; // help GC
			first = n;
			if (n == null)
			{
				last = null;
			}
			else
			{
				n.prev = null;
			}
			--count;
			notFull.signal();
			return item;
		}

		private E unlinkLast()
		{
			// assert lock.isHeldByCurrentThread();
			Node<E> l = last;
			if (l == null)
			{
				return default(E);
			}
			Node<E> p = l.prev;
			E item = l.item;
			l.item = default(E);
			l.prev = l; // help GC
			last = p;
			if (p == null)
			{
				first = null;
			}
			else
			{
				p.next = null;
			}
			--count;
			notFull.signal();
			return item;
		}

		private void unlink(Node<E> x)
		{
			// assert lock.isHeldByCurrentThread();
			Node<E> p = x.prev;
			Node<E> n = x.next;
			if (p == null)
			{
				unlinkFirst();
			}
			else if (n == null)
			{
				unlinkLast();
			}
			else
			{
				p.next = n;
				n.prev = p;
				x.item = default(E);
				// Don't mess with x's links.  They may still be in use by
				// an iterator.
			--count;
				notFull.signal();
			}
		}

		public void addFirst(object e)
		{
			if (!offerFirst(e))
			{
				throw new java.lang.IllegalStateException("Deque full");
			}
		}

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
        public void addLast(object e)
		{
			if (!offerLast(e))
			{
				throw new java.lang.IllegalStateException("Deque full");
			}
		}

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
        public bool offerFirst(object e)
		{
			if (e == null)
			{
				throw new System.NullReferenceException();
			}
			_lock.@lock();
			try
			{
				return linkFirst((E)e);
			}
			finally
			{
				_lock.unlock();
			}
		}

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
        public bool offerLast(object e)
		{
			if (e == null)
			{
				throw new System.NullReferenceException();
			}
			_lock.@lock();
			try
			{
				return linkLast((E)e);
			}
			finally
			{
				_lock.unlock();
			}
		}

		/// <summary>
		/// Links the provided element as the first in the queue, waiting until there
		/// is space to do so if the queue is full.
		/// </summary>
		/// <param name="e"> element to link
		/// </param>
		/// <exception cref="NullPointerException"> </exception>
		/// <exception cref="InterruptedException"> </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public void putFirst(E e) throws InterruptedException
		public virtual void putFirst(E e)
		{
			if (e == null)
			{
				throw new System.NullReferenceException();
			}
			_lock.@lock();
			try
			{
				while (!linkFirst(e))
				{
					notFull.@await();
				}
			}
			finally
			{
				_lock.unlock();
			}
		}

		/// <summary>
		/// Links the provided element as the last in the queue, waiting until there
		/// is space to do so if the queue is full.
		/// </summary>
		/// <param name="e"> element to link
		/// </param>
		/// <exception cref="NullPointerException"> </exception>
		/// <exception cref="InterruptedException"> </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public void putLast(E e) throws InterruptedException
		public virtual void putLast(E e)
		{
			if (e == null)
			{
				throw new System.NullReferenceException();
			}
			_lock.@lock();
			try
			{
				while (!linkLast(e))
				{
					notFull.@await();
				}
			}
			finally
			{
				_lock.unlock();
			}
		}

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public boolean offerFirst(E e, long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException
		public virtual bool offerFirst(E e, long timeout, TimeUnit unit)
		{
			if (e == null)
			{
				throw new System.NullReferenceException();
			}
			long nanos = unit.toNanos(timeout);
			_lock.lockInterruptibly();
			try
			{
				while (!linkFirst(e))
				{
					if (nanos <= 0)
					{
						return false;
					}
					nanos = notFull.awaitNanos(nanos);
				}
				return true;
			}
			finally
			{
				_lock.unlock();
			}
		}

		/// <summary>
		/// Links the provided element as the last in the queue, waiting up to the
		/// specified time to do so if the queue is full.
		/// </summary>
		/// <param name="e">         element to link </param>
		/// <param name="timeout">   length of time to wait </param>
		/// <param name="unit">      units that timeout is expressed in
		/// </param>
		/// <returns> {@code true} if successful, otherwise {@code false}
		/// </returns>
		/// <exception cref="NullPointerException"> </exception>
		/// <exception cref="InterruptedException"> </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public boolean offerLast(E e, long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException
		public virtual bool offerLast(E e, long timeout, TimeUnit unit)
		{
			if (e == null)
			{
				throw new System.NullReferenceException();
			}
			long nanos = unit.toNanos(timeout);
			_lock.lockInterruptibly();
			try
			{
				while (!linkLast(e))
				{
					if (nanos <= 0)
					{
						return false;
					}
					nanos = notFull.awaitNanos(nanos);
				}
				return true;
			}
			finally
			{
				_lock.unlock();
			}
		}

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
		public object removeFirst()
		{
			object x = pollFirst();
			if (x == null)
			{
				throw new NoSuchElementException();
			}
			return x;
		}

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
		public object removeLast()
		{
			object x = pollLast();
			if (x == null)
			{
				throw new NoSuchElementException();
			}
			return x;
		}

        public object pollFirst()
		{
			_lock.@lock();
			try
			{
				return unlinkFirst();
			}
			finally
			{
				_lock.unlock();
			}
		}

        public object pollLast()
		{
			_lock.@lock();
			try
			{
				return unlinkLast();
			}
			finally
			{
				_lock.unlock();
			}
		}

		/// <summary>
		/// Unlinks the first element in the queue, waiting until there is an element
		/// to unlink if the queue is empty.
		/// </summary>
		/// <returns> the unlinked element </returns>
		/// <exception cref="InterruptedException"> if the current thread is interrupted </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public E takeFirst() throws InterruptedException
		public virtual E takeFirst()
		{
			_lock.@lock();
			try
			{
				E x;
				while ((x = unlinkFirst()) == null)
				{
					notEmpty.@await();
				}
				return x;
			}
			finally
			{
				_lock.unlock();
			}
		}

		/// <summary>
		/// Unlinks the last element in the queue, waiting until there is an element
		/// to unlink if the queue is empty.
		/// </summary>
		/// <returns> the unlinked element </returns>
		/// <exception cref="InterruptedException"> if the current thread is interrupted </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public E takeLast() throws InterruptedException
		public virtual E takeLast()
		{
			_lock.@lock();
			try
			{
				E x;
				while ((x = unlinkLast()) == null)
				{
					notEmpty.@await();
				}
				return x;
			}
			finally
			{
				_lock.unlock();
			}
		}

		/// <summary>
		/// Unlinks the first element in the queue, waiting up to the specified time
		/// to do so if the queue is empty.
		/// </summary>
		/// <param name="timeout">   length of time to wait </param>
		/// <param name="unit">      units that timeout is expressed in
		/// </param>
		/// <returns> the unlinked element </returns>
		/// <exception cref="InterruptedException"> if the current thread is interrupted </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public E pollFirst(long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException
		public virtual E pollFirst(long timeout, TimeUnit unit)
		{
			long nanos = unit.toNanos(timeout);
			_lock.lockInterruptibly();
			try
			{
				E x;
				while ((x = unlinkFirst()) == null)
				{
					if (nanos <= 0)
					{
						return default(E);
					}
					nanos = notEmpty.awaitNanos(nanos);
				}
				return x;
			}
			finally
			{
				_lock.unlock();
			}
		}

		/// <summary>
		/// Unlinks the last element in the queue, waiting up to the specified time
		/// to do so if the queue is empty.
		/// </summary>
		/// <param name="timeout">   length of time to wait </param>
		/// <param name="unit">      units that timeout is expressed in
		/// </param>
		/// <returns> the unlinked element </returns>
		/// <exception cref="InterruptedException"> if the current thread is interrupted </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public E pollLast(long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException
		public virtual E pollLast(long timeout, TimeUnit unit)
		{
			long nanos = unit.toNanos(timeout);
			_lock.lockInterruptibly();
			try
			{
				E x;
				while ((x = unlinkLast()) == null)
				{
					if (nanos <= 0)
					{
						return default(E);
					}
					nanos = notEmpty.awaitNanos(nanos);
				}
				return x;
			}
			finally
			{
				_lock.unlock();
			}
		}

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
		public object getFirst()
		{
			object x = peekFirst();
			if (x == null)
			{
				throw new NoSuchElementException();
			}
			return x;
		}

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
		public object getLast()
		{
			object x = peekLast();
			if (x == null)
			{
				throw new NoSuchElementException();
			}
			return x;
		}

		public object peekFirst()
		{
			_lock.@lock();
			try
			{
				return (first == null) ? default(E) : first.item;
			}
			finally
			{
				_lock.unlock();
			}
		}

		public object peekLast()
		{
			_lock.@lock();
			try
			{
				return (last == null) ? default(E) : last.item;
			}
			finally
			{
				_lock.unlock();
			}
		}

		public bool removeFirstOccurrence(object o)
		{
			if (o == null)
			{
				return false;
			}
			_lock.@lock();
			try
			{
				for (Node<E> p = first; p != null; p = p.next)
				{
					if (o.Equals(p.item))
					{
						unlink(p);
						return true;
					}
				}
				return false;
			}
			finally
			{
				_lock.unlock();
			}
		}

		public bool removeLastOccurrence(object o)
		{
			if (o == null)
			{
				return false;
			}
			_lock.@lock();
			try
			{
				for (Node<E> p = last; p != null; p = p.prev)
				{
					if (o.Equals(p.item))
					{
						unlink(p);
						return true;
					}
				}
				return false;
			}
			finally
			{
				_lock.unlock();
			}
		}

		// BlockingQueue methods

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
		public override bool add(object e)
		{
			addLast(e);
			return true;
		}

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
		public override bool offer(object e)
		{
			return offerLast(e);
		}

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public void put(E e) throws InterruptedException
		public virtual void put(E e)
		{
			putLast(e);
		}

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public boolean offer(E e, long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException
		public virtual bool offer(E e, long timeout, TimeUnit unit)
		{
			return offerLast(e, timeout, unit);
		}

		public override object remove()
		{
			return removeFirst();
		}

		public override object poll()
		{
			return pollFirst();
		}

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public E take() throws InterruptedException
		public virtual E take()
		{
			return takeFirst();
		}

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public E poll(long timeout, java.util.concurrent.TimeUnit unit) throws InterruptedException
		public virtual E poll(long timeout, TimeUnit unit)
		{
			return pollFirst(timeout, unit);
		}

		public override object element()
		{
			return getFirst();
		}

		public override object peek()
		{
			return peekFirst();
		}

		public virtual int remainingCapacity()
		{
			_lock.@lock();
			try
			{
				return capacity - count;
			}
			finally
			{
				_lock.unlock();
			}
		}

		public virtual int drainTo(java.util.Collection c)
		{
			return drainTo(c, int.MaxValue);
		}

		public virtual int drainTo(java.util.Collection c, int maxElements)
		{
			if (c == null)
			{
				throw new System.NullReferenceException();
			}
			if (c == this)
			{
				throw new System.ArgumentException();
			}
			_lock.@lock();
			try
			{
				int n = System.Math.Min(maxElements, count);
				for (int i = 0; i < n; i++)
				{
					c.add(first.item); // In this order, in case add() throws.
					unlinkFirst();
				}
				return n;
			}
			finally
			{
				_lock.unlock();
			}
		}

		// Stack methods

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
		public void push(object e)
		{
			addFirst((E)e);
		}

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
		public object pop()
		{
			return removeFirst();
		}

		public override bool remove(object o)
		{
			return removeFirstOccurrence(o);
		}

		/// <summary>
		/// Returns the number of elements in this deque.
		/// </summary>
		/// <returns> the number of elements in this deque </returns>
		public override int size()
		{
			_lock.@lock();
			try
			{
				return count;
			}
			finally
			{
				_lock.unlock();
			}
		}


		public override bool contains(object o)
		{
			if (o == null)
			{
				return false;
			}
			_lock.@lock();
			try
			{
				for (Node<E> p = first; p != null; p = p.next)
				{
					if (o.Equals(p.item))
					{
						return true;
					}
				}
				return false;
			}
			finally
			{
				_lock.unlock();
			}
		}

		public override object[] toArray()
		{
			_lock.@lock();
			try
			{
				object[] a = new object[count];
				int k = 0;
				for (Node<E> p = first; p != null; p = p.next)
				{
					a[k++] = p.item;
				}
				return a;
			}
			finally
			{
				_lock.unlock();
			}
		}

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @SuppressWarnings("unchecked") @Override public <T> T[] toArray(T[] a)
		public override object[] toArray(object[] a)
		{
			_lock.@lock();
			try
			{
				if (a.Length < count)
				{
					a = new object[count];
				}
				int k = 0;
				for (Node<E> p = first; p != null; p = p.next)
				{
					a[k++] = p.item;
				}
				if (a.Length > k)
				{
					a[k] = null;
				}
				return a;
			}
			finally
			{
				_lock.unlock();
			}
		}

        public new string ToString()
		{
			_lock.@lock();
			try
			{
				return base.ToString();
			}
			finally
			{
				_lock.unlock();
			}
		}

		public override void clear()
		{
			_lock.@lock();
			try
			{
				for (Node<E> f = first; f != null;)
				{
					f.item = default(E);
					Node<E> n = f.next;
					f.prev = null;
					f.next = null;
					f = n;
				}
				first = last = null;
				count = 0;
				notFull.signalAll();
			}
			finally
			{
				_lock.unlock();
			}
		}

        public override java.util.Iterator iterator()
		{
			return new Itr(this);
		}

		/// <summary>
		/// {@inheritDoc}
		/// </summary>
		public java.util.Iterator descendingIterator()
		{
			return new DescendingItr(this);
		}

		/// <summary>
		/// Base class for Iterators for LinkedBlockingDeque
		/// </summary>
        private abstract class AbstractItr : java.util.Iterator
		{
			protected readonly LinkedBlockingDeque<E> outerInstance;


			 internal Node<E> next_Renamed;

			internal E nextItem;

			internal Node<E> lastRet;

			internal abstract Node<E> firstNode();

			internal abstract Node<E> nextNode(Node<E> n);

			/// <summary>
			/// Create a new iterator. Sets the initial position.
			/// </summary>
			internal AbstractItr(LinkedBlockingDeque<E> outerInstance)
			{
				this.outerInstance = outerInstance;
				// set to initial position
				outerInstance._lock.@lock();
				try
				{
					next_Renamed = firstNode();
					nextItem = (next_Renamed == null) ? default(E) : next_Renamed.item;
				}
				finally
				{
					outerInstance._lock.unlock();
				}
			}

			/// <summary>
			/// Advances next.
			/// </summary>
			internal virtual void advance()
			{
				outerInstance._lock.@lock();
				try
				{
					// assert next != null;
					Node<E> s = nextNode(next_Renamed);
					if (s == next_Renamed)
					{
						next_Renamed = firstNode();
					}
					else
					{
						// Skip over removed nodes.
						// May be necessary if multiple interior Nodes are removed.
						while (s != null && s.item == null)
						{
							s = nextNode(s);
						}
						next_Renamed = s;
					}
					nextItem = (next_Renamed == null) ? default(E) : next_Renamed.item;
				}
				finally
				{
					outerInstance._lock.unlock();
				}
			}

			public bool hasNext()
			{
				return next_Renamed != null;
			}

			public object next()
			{
				if (next_Renamed == null)
				{
					throw new NoSuchElementException();
				}
				lastRet = next_Renamed;
				E x = nextItem;
				advance();
				return x;
			}

			public void remove()
			{
				Node<E> n = lastRet;
				if (n == null)
				{
					throw new java.lang.IllegalStateException();
				}
				lastRet = null;
				outerInstance._lock.@lock();
				try
				{
					if (n.item != null)
					{
						outerInstance.unlink(n);
					}
				}
				finally
				{
					outerInstance._lock.unlock();
				}
			}
		}

		/// <summary>
		/// Forward iterator </summary>
		private class Itr : AbstractItr
		{
			public Itr(LinkedBlockingDeque<E> outerInstance) : base(outerInstance)
			{
			}

			internal override Node<E> firstNode()
			{
				return outerInstance.first;
			}
			internal override Node<E> nextNode(Node<E> n)
			{
				return n.next;
			}
		}

		/// <summary>
		/// Descending iterator </summary>
		private class DescendingItr : AbstractItr
		{
			public DescendingItr(LinkedBlockingDeque<E> outerInstance) : base(outerInstance)
			{
			}

			internal override Node<E> firstNode()
			{
				return outerInstance.last;
			}
			internal override Node<E> nextNode(Node<E> n)
			{
				return n.prev;
			}
		}

		private void writeObject(java.io.ObjectOutputStream s)
		{
			_lock.@lock();
			try
			{
				// Write out capacity and any hidden stuff
				s.defaultWriteObject();
				// Write out all elements in the proper order.
				for (Node<E> p = first; p != null; p = p.next)
				{
					s.writeObject(p.item);
				}
				// Use trailing null as sentinel
				s.writeObject(null);
			}
			finally
			{
				_lock.unlock();
			}
		}

		private void readObject(java.io.ObjectInputStream s)
		{
			s.defaultReadObject();
			count = 0;
			first = null;
			last = null;
			// Read in all elements and place in queue
			for (;;)
			{
				E item = (E)s.readObject();
				if (item == null)
				{
					break;
				}
				add(item);
			}
		}

		public virtual bool hasTakeWaiters()
		{
			_lock.@lock();
			try
			{
				return _lock.hasWaiters(notEmpty);
			}
			finally
			{
				_lock.unlock();
			}
		}

		public virtual int TakeQueueLength
		{
			get
			{
				_lock.@lock();
				try
				{
				   return _lock.getWaitQueueLength(notEmpty);
				}
				finally
				{
					_lock.unlock();
				}
			}
		}

		public virtual void interuptTakeWaiters()
		{
			_lock.@lock();
			try
			{
			   _lock.interruptWaiters(notEmpty);
			}
			finally
			{
				_lock.unlock();
			}
		}
	}

}