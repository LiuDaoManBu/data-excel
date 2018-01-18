package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.constant.Necessity;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;

public class MenuConfig {
  public static class Builder {
    private TableConfig tableConfig;
    private MenuConfig parent;
    private MenuDataConfig.Builder dataConfigBuilder;
    private List<MenuConfig.Builder> childrenBuilders;
    // 菜单匹配器
    private Matcher.Builder<StandardCell> matcherBuilder;
    // 第一个数据单元格相对于菜单单元格的单元格距离
    private Integer distance;
    private Necessity necessity;
    private Direction direction;
    // TODO need enum?
    private ParserConfig parserConfig;

    public MenuConfig build() {
      distance = Optional.ofNullable(distance).orElse(DEFAULT_DISTANCE);
      necessity = Optional.ofNullable(necessity).orElse(DEFAULT_MENU_NECESSITY);


//      tableConfig = Optional.ofNullable(tableConfig).orElse(Optional
//          .ofNullable(parent).map(MenuConfig::getTableConfig).orElse(null));
      direction = Optional.ofNullable(direction).orElse(
          Optional.ofNullable(parent).map(MenuConfig::getDirection).orElse(DEFAULT_DIRECTION));
      childrenBuilders = Optional.ofNullable(childrenBuilders).orElse(ImmutableList.of());
      // TODO tip
      Preconditions.checkState(Objects.nonNull(tableConfig));
      Preconditions.checkNotNull(matcherBuilder);
      Preconditions.checkNotNull(necessity);
      // Preconditions.checkState(Objects.nonNull(direction));
//      Preconditions.checkNotNull(menuType);
      Preconditions.checkState(
          !(!Iterables.isEmpty(childrenBuilders) && Objects.nonNull(dataConfigBuilder)));
      return new MenuConfig(this);
    }

    public TableConfig getTableConfig() {
      return tableConfig;
    }

    public Builder setTableConfig(TableConfig tableConfig) {
      this.tableConfig = tableConfig;
      return this;
    }

    public int getDistance() {
      return distance;
    }

    public Necessity getNecessity() {
      return necessity;
    }

    public Builder setNecessity(Necessity necessity) {
      this.necessity = necessity;
      return this;
    }

    public Direction getDirection() {
      return direction;
    }

    public Builder setDirection(Direction direction) {
      this.direction = direction;
      return this;
    }

    public MenuConfig getParent() {
      return parent;
    }

    public Builder setParent(MenuConfig parent) {
      this.parent = parent;
      return this;
    }

    public MenuDataConfig.Builder getDataConfigBuilder() {
      return dataConfigBuilder;
    }

    public Builder setDataConfigBuilder(MenuDataConfig.Builder dataConfigBuilder) {
      this.dataConfigBuilder = dataConfigBuilder;
      return this;
    }

    public List<MenuConfig.Builder> getChildrenBuilders() {
      return childrenBuilders;
    }

    public Builder setChildrenBuilders(List<MenuConfig.Builder> childrenBuilders) {
      this.childrenBuilders = childrenBuilders;
      return this;
    }


    public Matcher.Builder<StandardCell> getMatcherBuilder() {
      return matcherBuilder;
    }

    public Builder setMatcherBuilder(Matcher.Builder<StandardCell> matcherBuilder) {
      this.matcherBuilder = matcherBuilder;
      return this;
    }

    public Builder setDistance(Integer distance) {
      this.distance = distance;
      return this;
    }

    public ParserConfig getParserConfig() {
      return parserConfig;
    }

    public Builder setParserConfig(ParserConfig parserConfig) {
      this.parserConfig = parserConfig;
      return this;
    }

  }

  public static final int DEFAULT_DISTANCE = 1;
  private static final Necessity DEFAULT_MENU_NECESSITY = Necessity.MUST;
  public static final Direction DEFAULT_DIRECTION = Direction.BOTTOM;

  public static  Builder builder() {
    return new Builder();
  }

  private final TableConfig tableConfig;
  // 菜单匹配器
  private final Matcher<StandardCell> menuMatcher;
  // 第一个数据单元格相对于菜单单元格的单元格距离
  private final int distance;
  private final Necessity necessity;
  private final Direction direction;
  private final MenuConfig parent;
  private final ImmutableCollection<MenuConfig> childrens;
  private final MenuDataConfig dataConfig;
  private final ParserConfig parserConfig;

  private MenuConfig(Builder builder) {
    tableConfig = builder.tableConfig;
    menuMatcher = builder.matcherBuilder.build();
    distance = builder.distance;
    necessity = builder.necessity;
    direction = builder.direction;
    parent = builder.parent;
    childrens = builder.childrenBuilders.stream()
        .peek(childrenBuilder -> childrenBuilder.setParent(this).setTableConfig(tableConfig))
        .map(Builder::build).collect(ImmutableSet.toImmutableSet());
    dataConfig = builder.dataConfigBuilder.setMenuConfig(this).build();
    parserConfig = builder.parserConfig;
  }

  public Optional<Field> getField() {
    return Optional.ofNullable(dataConfig).map(MenuDataConfig::getField);
  }

  public Optional<String> getFieldName() {
    return Optional.ofNullable(dataConfig).map(MenuDataConfig::getFieldName);
  }

  public boolean isTopMenu() {
    return Objects.isNull(parent);
  }

  public boolean isMustMenu() {
    return Necessity.MUST.equals(necessity);
  }

  public boolean isNotMustMenu() {
    return Necessity.NOT_MUST.equals(necessity);
  }

  public boolean isDataMenu() {
    return childrens.isEmpty();
  }

  public boolean isSingleDataMenu() {
    return isDataMenu() && LoadType.SINGLE.equals(dataConfig.getLoadType());
  }
  
//  public boolean isFixedDataMenu() {
//    return isDataMenu() && LoadType.FIXED.equals(dataConfig.getLoadType());
//  }

  public boolean isUnFixedDataMenu() {
    return isDataMenu() && LoadType.UNFIXED.equals(dataConfig.getLoadType());
  }

  public ParserConfig getEffectiveParserConfig() {
    return Optional.ofNullable(parserConfig).orElse(Optional.ofNullable(parent)
        .map(MenuConfig::getParserConfig).orElse(tableConfig.getEffectiveParserConfig()));
  }

  // delegate methods start

  public boolean matches(StandardCell cell) {
    return menuMatcher.test(cell);
  }

  // delegate methods end

  public Matcher<StandardCell> getMenuMatcher() {
    return menuMatcher;
  }

  public int getDistance() {
    return distance;
  }

  public MenuConfig getParent() {
    return parent;
  }

  public Direction getDirection() {
    return direction;
  }

  public Necessity getNecessity() {
    return necessity;
  }

  public MenuDataConfig getDataConfig() {
    return dataConfig;
  }

  public ImmutableCollection<MenuConfig> getChildrens() {
    return childrens;
  }

  public TableConfig getTableConfig() {
    return tableConfig;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

}
