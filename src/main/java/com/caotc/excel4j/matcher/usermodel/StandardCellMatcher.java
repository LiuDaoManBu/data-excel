package com.caotc.excel4j.matcher.usermodel;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import org.apache.poi.hssf.util.CellReference;
import org.apache.poi.ss.usermodel.CellType;
import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.StringMatcher.Builder.Expression;
import com.caotc.excel4j.matcher.constant.ComparableMatcherType;
import com.caotc.excel4j.matcher.constant.StringMatcherType;
import com.caotc.excel4j.matcher.data.type.BaseDataType;
import com.caotc.excel4j.matcher.data.type.DataType;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Preconditions;
import com.google.common.reflect.TypeToken;

public class StandardCellMatcher extends BaseMatcher<StandardCell> {
  public static class Builder extends BaseMatcher.Builder<StandardCell> {
    private BaseDataType baseDataType;
    private DataType dataType;
    //TODO 不当做String类型对待扩展?
    private List<Expression> nameExpressions;

    @Override
    public StandardCellMatcher build() {
      dataType=Stream.of(dataType,baseDataType).filter(Objects::nonNull).findFirst().orElse(DEFAULT_DATA_TYPE);
    //TODO tip
      Preconditions.checkState(Objects.nonNull(dataType));
      
      //TODO 子类
    if (Objects.nonNull(nameExpressions)) {
      nameExpressions.stream().forEach(expression -> add(expression.getMatcherType(),
          expression.getPredicateValue(), standardCell->dataType.cast(standardCell.getValue(), String.class)));
    }
      return new StandardCellMatcher(this);
    }

    public Builder addDataPredicate(Predicate<Object> predicate) {
      add(predicate, StandardCell::getValue);
      return this;
    }

    public <T> Builder addDataPredicate(Predicate<T> predicate, TypeToken<T> type) {
      add(predicate, cell -> dataType.cast(cell.getValue(), type));
      return this;
    }

    public Builder addDataPredicate(StringMatcherType type, String predicateValue) {
      add(type, predicateValue, cell ->dataType.cast(cell.getValue(), String.class));
      return this;
    }

    public <T extends Comparable<T>> Builder addDataPredicate(ComparableMatcherType type,
        T predicateValue) {
      // TODO safe?
      add(type, predicateValue, cell -> (T) dataType.cast(cell.getValue(), predicateValue.getClass()));
      return this;
    }

    public Builder addCellTypePredicate(Predicate<CellType> predicate) {
      add(predicate, StandardCell::getCellTypeEnum);
      return this;
    }

    public Builder addFirstRowIndexPredicate(ComparableMatcherType type,
        int predicateValue) {
      add(type, predicateValue, StandardCell::getFirstRow);
      return this;
    }

    public Builder addLastRowIndexPredicate(ComparableMatcherType type,
        int predicateValue) {
      add(type, predicateValue, StandardCell::getLastRow);
      return this;
    }

    public Builder addFirstColumnIndexPredicate(ComparableMatcherType type,
        int predicateValue) {
      add(type, predicateValue, StandardCell::getFirstColumn);
      return this;
    }

    public Builder addLastColumnIndexPredicate(ComparableMatcherType type,
        int predicateValue) {
      add(type, predicateValue, StandardCell::getLastColumn);
      return this;
    }

    public Builder addFirstColumnIndexStringPredicate(StringMatcherType type,
        String predicateValue) {
      Function<StandardCell, Integer> f = StandardCell::getFirstColumn;
      add(type, predicateValue, f.andThen(CellReference::convertNumToColString));
      return this;
    }

    public Builder addLastColumnIndexStringPredicate(StringMatcherType type,
        String predicateValue) {
      Function<StandardCell, Integer> f = StandardCell::getLastColumn;
      add(type, predicateValue, f.andThen(CellReference::convertNumToColString));
      return this;
    }
    
    public BaseDataType getBaseDataType() {
      return baseDataType;
    }

    public Builder setBaseDataType(BaseDataType baseDataType) {
      this.baseDataType = baseDataType;
      return this;
    }

    public DataType getDataType() {
      return dataType;
    }

    public Builder setDataType(DataType dataType) {
      this.dataType = dataType;
      return this;
    }

    public List<Expression> getNameExpressions() {
      return nameExpressions;
    }

    public Builder setNameExpressions(List<Expression> nameExpressions) {
      this.nameExpressions = nameExpressions;
      return this;
    }
    
  }
  
  private static final DataType DEFAULT_DATA_TYPE=BaseDataType.STRING;
  
  public static Builder builder() {
    return new Builder();
  }
  
  private final DataType dataType;
  private StandardCellMatcher(Builder builder) {
    super(builder);
    this.dataType=builder.dataType;
  }
  public DataType getDataType() {
    return dataType;
  }
}
