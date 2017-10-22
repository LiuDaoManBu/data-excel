package com.caotc.excel4j.collect;

import java.util.Collections;
import java.util.Iterator;
import com.google.common.base.Function;
import com.google.common.base.Preconditions;
import com.google.common.collect.AbstractIterator;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.Iterables;
import com.google.common.collect.TreeTraverser;

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

  public Iterable<? extends Iterable<T>> breadth() {
    FluentIterable<? extends Iterable<T>> fluentIterable =
        FluentIterable.from(Collections.singleton(Collections.singleton(root)));
    for (Iterable<T> last = Iterables.getLast(fluentIterable); Iterables
        .isEmpty(Iterables.transform(last, this::children)); FluentIterable
            .concat(Iterables.transform(last, this::children))) {

    }
    return FluentIterable.from(Collections.singleton(Collections.singleton(root)));
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
