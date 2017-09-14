package com.caotc.excel4j.util;

import java.util.Collection;
import com.caotc.excel4j.matcher.Matcher;

public class MatcherUtil {
  public static boolean allSupport(Collection<? extends Matcher> matchers, Object value) {
    return matchers.stream().allMatch(matcher -> matcher.support(value));
  }

  public static boolean allMatches(Collection<? extends Matcher> matchers, Object value) {
    return matchers.stream().allMatch(matcher -> matcher.matches(value));
  }

  public static boolean anyMatches(Collection<Collection<? extends Matcher>> matchers,
      Object value) {
    return matchers.stream().anyMatch(matcher -> allMatches(matcher, value));
  }
}
