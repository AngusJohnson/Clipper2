/*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  10 October 2024                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  A pool of reusable vertex objects.                    *
* Thanks    :  Special thanks to Thong Nguyen, Guus Kuiper, Phil Stopford,     *
*           :  and Daniel Gosnell for their invaluable assistance with C#.     *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************/

using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Clipper2Lib
{
  /// <summary>
  /// A pool of reusable vertex objects
  /// </summary>
  internal class VertexPoolList : PooledList<Vertex>
  {
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public Vertex Add(Point64 point, VertexFlags flags, Vertex? prev)
    {
      TryGrow();
      Vertex poolVtx = _items[_size];
      if (poolVtx == null)
      {
        poolVtx = new Vertex(point, flags, prev);
        _items[_size] = poolVtx;
      }
      else
      {
        //reuse already allocated vertex
        poolVtx.pt = point;
        poolVtx.flags = flags;
        poolVtx.prev = prev;
        poolVtx.next = null;
      }
      _size++;
      return poolVtx;
    }
  }

  /// <summary>
  /// A List that pools objects added to it for reuse.
  /// Indexing, growing and enumeration implementation is identical to <see cref="System.Collections.Generic.List{T}"/>.
  /// The pooled list reuses allocated reference objects. Operations are limited to read, add and clear.
  /// </summary>
  internal abstract class PooledList<T> : IReadOnlyList<T> where T : class
  {
    private const int DefaultCapacity = 4;

    protected T[] _items;
    protected int _size;

    public T this[int index]
    {
      get
      {
        if ((uint) index >= (uint) _size)
        {
          throw new ArgumentOutOfRangeException("index must be greater or equal to zero and less than the size of the collection");
        };
        return _items[index];
      }
    }

    public int Count => _size;

    public PooledList()
    {
      _items = Array.Empty<T>();
    }

    public PooledList(int capacity)
    {
      _items = new T[capacity];
    }

    public int Capacity
    {
      get => _items.Length;
      set
      {
        if (value > _items.Length)
        {
          T[] newItems = new T[value];
          if (_size > 0)
          {
            Array.Copy(_items, newItems, _size);
          }
          _items = newItems;
        }
      }
    }

    public void EnsureCapacity(int capacity) { Capacity = capacity; }

    protected void TryGrow()
    {
      int newSize = _size + 1;
      if (newSize > _items.Length)
      {
        //grow the array
        int newCapacity = _items.Length == 0 ? DefaultCapacity : 2 * _items.Length;
        Capacity = newCapacity;
      }
    }

    public void Clear()
    {
      //unlike List<T>, DO NOT null the objects in the list, even if they are reference types. We reuse them
      _size = 0;
    }

    public IEnumerator<T> GetEnumerator()
    {
      return new ListEnumerator<T>(this);
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
      return new ListEnumerator<T>(this);
    }

    private struct ListEnumerator<T2> : IEnumerator<T2> where T2 : class
    {
      private PooledList<T2> _list;
      private int _index;
      private T2? _current;
      
      public ListEnumerator(PooledList<T2> list)
      {
        _list = list;
        _index = 0;
        _current = default;
      }

      public T2 Current => _current!;

      object IEnumerator.Current => _current!;

      public void Dispose()
      {
      }

      public bool MoveNext()
      {
        int count = _list._size;
        if ((uint) _index < (uint) count)
        {
          _current = _list[_index];
          _index++;
          return true;
        }
        return MoveNextRare(count);
      }

      private bool MoveNextRare(int count)
      {
        _index = count + 1;
        _current = default;
        return false;
      }

      public void Reset()
      {
        _index = 0;
        _current = default;
      }
    }
  }
} // namespace