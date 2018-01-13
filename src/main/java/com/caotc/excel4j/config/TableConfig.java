package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.matcher.usermodel.TableMatcher;
import com.caotc.excel4j.parse.result.Table;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Streams;
import com.google.common.graph.SuccessorsFunction;
import com.google.common.graph.Traverser;

// TODO classType?
public class TableConfig {
  public static class Builder {
    private List<MenuConfig.Builder<?>> topMenuConfigBuilders;
    private SheetConfig sheetConfig;
    private TableMatcher.Builder matcherBuilder;
    private Direction fixedMenuDirection;
    private Direction unFixedMenuDirection;
    private ParserConfig parserConfig;

    public TableConfig builder() {
      return new TableConfig(this);
    }

    public SheetConfig getSheetConfig() {
      return sheetConfig;
    }

    public Builder setSheetConfig(SheetConfig sheetConfig) {
      this.sheetConfig = sheetConfig;
      return this;
    }

    public Direction getFixedMenuDirection() {
      return fixedMenuDirection;
    }

    public Builder setFixedMenuDirection(Direction fixedMenuDirection) {
      this.fixedMenuDirection = fixedMenuDirection;
      return this;
    }

    public Direction getUnFixedMenuDirection() {
      return unFixedMenuDirection;
    }

    public Builder setUnFixedMenuDirection(Direction unFixedMenuDirection) {
      this.unFixedMenuDirection = unFixedMenuDirection;
      return this;
    }

    public List<MenuConfig.Builder<?>> getTopMenuConfigBuilders() {
      return topMenuConfigBuilders;
    }

    public Builder setTopMenuConfigBuilders(List<MenuConfig.Builder<?>> topMenuConfigBuilders) {
      this.topMenuConfigBuilders = topMenuConfigBuilders;
      return this;
    }

    public ParserConfig getParserConfig() {
      return parserConfig;
    }

    public Builder setParserConfig(ParserConfig parserConfig) {
      this.parserConfig = parserConfig;
      return this;
    }

    public TableMatcher.Builder getMatcherBuilder() {
      return matcherBuilder;
    }

    public Builder setMatcherBuilder(TableMatcher.Builder matcherBuilder) {
      this.matcherBuilder = matcherBuilder;
      return this;
    }

  }

  private static final Traverser<MenuConfig<?>> MENU_CONFIG_TRAVERSER =
      Traverser.forTree(new SuccessorsFunction<MenuConfig<?>>() {
        @Override
        public Iterable<? extends MenuConfig<?>> successors(MenuConfig<?> node) {
          return node.getChildrenMenuConfigs();
        }
      });

  public static <V> Builder builder() {
    return new Builder();
  }

  private final SheetConfig sheetConfig;
  private final Direction fixedMenuDirection;
  private final Direction unFixedMenuDirection;
  private final ImmutableCollection<MenuConfig<?>> topMenuConfigs;
  private final TableMatcher matcher;
  private final ParserConfig parserConfig;

  private TableConfig(Builder builder) {
    sheetConfig = builder.sheetConfig;
    fixedMenuDirection = builder.fixedMenuDirection;
    unFixedMenuDirection = builder.unFixedMenuDirection;
    matcher = builder.matcherBuilder.build();

    topMenuConfigs = builder.topMenuConfigBuilders.stream()
        .peek(topMenuConfigBuilder -> topMenuConfigBuilder.setTableConfig(this))
        .map(MenuConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
    parserConfig = builder.parserConfig;

    // menuConfigs =
    // builder.menuConfigBuilders.stream().map(MenuConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
  }

  public Table.Builder parse(Sheet sheet) {
    Table.Builder builder = Table.builder().setTableConfig(this);

    // if (matcher.test(sheet)) {
    // builder.setTableBuilders(tableBuilders);
    // } else {
    // builder.setErrors(ImmutableList.of(new SheetError(sheet, matcher.getMessage(sheet))));
    // }
    return builder;
  }

  public ParserConfig getEffectiveParserConfig() {
    return Optional.ofNullable(parserConfig).orElse(sheetConfig.getEffectiveParserConfig());
  }

  public Stream<MenuConfig<?>> getMenuConfigs() {
    return topMenuConfigs.stream().map(MENU_CONFIG_TRAVERSER::breadthFirst)
        .flatMap(Streams::stream);
  }

  public SheetConfig getSheetConfig() {
    return sheetConfig;
  }

  public ImmutableCollection<MenuConfig<?>> getTopMenuConfigs() {
    return topMenuConfigs;
  }

  public Direction getFixedMenuDirection() {
    return fixedMenuDirection;
  }

  public Direction getUnFixedMenuDirection() {
    return unFixedMenuDirection;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

  public TableMatcher getMatcher() {
    return matcher;
  }

}
