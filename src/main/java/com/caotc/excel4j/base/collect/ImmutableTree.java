package com.caotc.excel4j.base.collect;

import java.util.Collections;
import java.util.Iterator;
import java.util.NoSuchElementException;
import com.google.common.base.Function;
import com.google.common.base.Preconditions;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.Iterables;
import com.google.common.collect.TreeTraverser;
import com.google.common.collect.UnmodifiableIterator;

public class ImmutableTree<T> extends TreeTraverser<T> {
  public static <T> ImmutableTree<T> using(T root,
      Function<T, ? extends Iterable<T>> nodeToChildrenFunction) {
    Preconditions.checkNotNull(root);
    Preconditions.checkNotNull(nodeToChildrenFunction);
    return new ImmutableTree<T>(root, nodeToChildrenFunction);
  }

  private final T root;
  private final Function<T, ? extends Iterable<T>> nodeToChildrenFunction;

  private ImmutableTree(T root, Function<T, ? extends Iterable<T>> nodeToChildrenFunction) {
    super();
    this.root = root;
    this.nodeToChildrenFunction = nodeToChildrenFunction;
  }

  public Iterable<? extends Iterable<T>> breadthIterable(T root) {
    return Iterables.unmodifiableIterable(new Iterable<Iterable<T>>() {
      @Override
      public Iterator<Iterable<T>> iterator() {
        return new UnmodifiableIterator<Iterable<T>>() {
          private FluentIterable<T> iterable = FluentIterable.from(Collections.singleton(root));

          @Override
          public boolean hasNext() {
            return !iterable.isEmpty();
          }

          @Override
          public Iterable<T> next() {
            if (!hasNext()) {
              throw new NoSuchElementException();
            }
            FluentIterable<T> result = iterable;
            iterable = iterable.transformAndConcat(nodeToChildrenFunction);
            return result;
          }

        };
      }
    });
  }
  
  public Iterable<? extends Iterable<T>> breadthIterable() {
    return breadthIterable(root);
  }
  
  public FluentIterable<T> breadthFirstTraversal() {
    return breadthFirstTraversal(root);
  }

  public FluentIterable<T> postOrderTraversal() {
    return postOrderTraversal(root);
  }

  public FluentIterable<T> preOrderTraversal() {
    return preOrderTraversal(root);
  }

  @Override
  public Iterable<T> children(T root) {
    return nodeToChildrenFunction.apply(root);
  }

  public T getRoot() {
    return root;
  }

  public Function<T, ? extends Iterable<T>> getNodeToChildrenFunction() {
    return nodeToChildrenFunction;
  }

}
