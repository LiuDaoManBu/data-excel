package com.caotc.excel4j.expression.constant;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import com.googlecode.aviator.asm.Opcodes;

public enum CodeByteVersion {
  V1_1(Opcodes.V1_1), V1_2(Opcodes.V1_2), V1_3(Opcodes.V1_3), V1_4(Opcodes.V1_4), V1_5(
      Opcodes.V1_5), V1_6(Opcodes.V1_6);
  private static final Map<Integer, CodeByteVersion> VALUE_TO_VERSIONS =
      Arrays.asList(CodeByteVersion.values()).stream()
          .collect(Collectors.toMap(CodeByteVersion::getValue, Function.identity()));

  public static CodeByteVersion valueOf(int value) {
    return VALUE_TO_VERSIONS.get(value);
  }

  private final int value;

  private CodeByteVersion(int value) {
    this.value = value;
  }

  public int getValue() {
    return value;
  }

}
