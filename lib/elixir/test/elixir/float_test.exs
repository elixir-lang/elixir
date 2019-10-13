Code.require_file("test_helper.exs", __DIR__)

defmodule FloatTest do
  use ExUnit.Case, async: true

  doctest Float

  test "parse/1" do
    assert Float.parse("12") === {12.0, ""}
    assert Float.parse("-12") === {-12.0, ""}
    assert Float.parse("-0.1") === {-0.1, ""}
    assert Float.parse("123456789") === {123_456_789.0, ""}
    assert Float.parse("12.5") === {12.5, ""}
    assert Float.parse("12.524235") === {12.524235, ""}
    assert Float.parse("-12.5") === {-12.5, ""}
    assert Float.parse("-12.524235") === {-12.524235, ""}
    assert Float.parse("0.3534091") === {0.3534091, ""}
    assert Float.parse("0.3534091elixir") === {0.3534091, "elixir"}
    assert Float.parse("7.5e3") === {7.5e3, ""}
    assert Float.parse("7.5e-3") === {7.5e-3, ""}
    assert Float.parse("12x") === {12.0, "x"}
    assert Float.parse("12.5x") === {12.5, "x"}
    assert Float.parse("-12.32453e10") === {-1.232453e11, ""}
    assert Float.parse("-12.32453e-10") === {-1.232453e-9, ""}
    assert Float.parse("0.32453e-10") === {3.2453e-11, ""}
    assert Float.parse("1.32453e-10") === {1.32453e-10, ""}
    assert Float.parse("1.32.45") === {1.32, ".45"}
    assert Float.parse("1.o") === {1.0, ".o"}
    assert Float.parse("+12.3E+4") === {1.23e5, ""}
    assert Float.parse("+12.3E-4x") === {0.00123, "x"}
    assert Float.parse("-1.23e-0xFF") === {-1.23, "xFF"}
    assert Float.parse("-1.e2") === {-1.0, ".e2"}
    assert Float.parse(".12") === :error
    assert Float.parse("--1.2") === :error
    assert Float.parse("++1.2") === :error
    assert Float.parse("pi") === :error
    assert Float.parse("1.7976931348623157e308") === {1.7976931348623157e308, ""}

    assert_raise ArgumentError, fn ->
      Float.parse("1.7976931348623159e308")
    end
  end

  test "floor/1" do
    assert Float.floor(12.524235) === 12.0
    assert Float.floor(-12.5) === -13.0
    assert Float.floor(-12.524235) === -13.0
    assert Float.floor(7.5e3) === 7500.0
    assert Float.floor(7.5432e3) === 7543.0
    assert Float.floor(7.5e-3) === 0.0
    assert Float.floor(-12.32453e4) === -123_246.0
    assert Float.floor(-12.32453e-10) === -1.0
    assert Float.floor(0.32453e-10) === 0.0
    assert Float.floor(-0.32453e-10) === -1.0
    assert Float.floor(1.32453e-10) === 0.0
  end

  describe "floor/2" do
    test "with 0.0" do
      for precision <- 0..15 do
        assert Float.floor(0.0, precision) === 0.0
        assert Float.floor(-0.0, precision) === -0.0
      end
    end

    test "floor/2 with precision" do
      assert Float.floor(12.524235, 0) === 12.0
      assert Float.floor(-12.524235, 0) === -13.0

      assert Float.floor(12.52, 2) === 12.51
      assert Float.floor(-12.52, 2) === -12.52

      assert Float.floor(12.524235, 2) === 12.52
      assert Float.floor(-12.524235, 3) === -12.525

      assert Float.floor(12.32453e-20, 2) === 0.0
      assert Float.floor(-12.32453e-20, 2) === -0.01

      assert_raise ArgumentError, "precision 16 is out of valid range of 0..15", fn ->
        Float.floor(1.1, 16)
      end
    end

    test "with subnormal floats" do
      assert Float.floor(-5.0e-324, 0) === -1.0
      assert Float.floor(-5.0e-324, 1) === -0.1
      assert Float.floor(-5.0e-324, 2) === -0.01
      assert Float.floor(-5.0e-324, 15) === -0.000000000000001

      for precision <- 0..15 do
        assert Float.floor(5.0e-324, precision) === 0.0
      end
    end
  end

  test "ceil/1" do
    assert Float.ceil(12.524235) === 13.0
    assert Float.ceil(-12.5) === -12.0
    assert Float.ceil(-12.524235) === -12.0
    assert Float.ceil(7.5e3) === 7500.0
    assert Float.ceil(7.5432e3) === 7544.0
    assert Float.ceil(7.5e-3) === 1.0
    assert Float.ceil(-12.32453e4) === -123_245.0
    assert Float.ceil(-12.32453e-10) === 0.0
    assert Float.ceil(0.32453e-10) === 1.0
    assert Float.ceil(-0.32453e-10) === 0.0
    assert Float.ceil(1.32453e-10) === 1.0
    assert Float.ceil(0.0) === 0.0
  end

  describe "ceil/2" do
    test "with 0.0" do
      for precision <- 0..15 do
        assert Float.ceil(0.0, precision) === 0.0
        assert Float.ceil(-0.0, precision) === -0.0
      end
    end

    test "with regular floats" do
      assert Float.ceil(12.524235, 0) === 13.0
      assert Float.ceil(-12.524235, 0) === -12.0

      assert Float.ceil(12.52, 2) === 12.52
      assert Float.ceil(-12.52, 2) === -12.51

      assert Float.ceil(12.524235, 2) === 12.53
      assert Float.ceil(-12.524235, 3) === -12.524

      assert Float.ceil(12.32453e-20, 2) === 0.01
      assert Float.ceil(-12.32453e-20, 2) === 0.0

      assert Float.ceil(0.0, 2) === 0.0

      assert_raise ArgumentError, "precision 16 is out of valid range of 0..15", fn ->
        Float.ceil(1.1, 16)
      end
    end

    test "with subnormal floats" do
      assert Float.ceil(5.0e-324, 0) === 1.0
      assert Float.ceil(5.0e-324, 1) === 0.1
      assert Float.ceil(5.0e-324, 2) === 0.01
      assert Float.ceil(5.0e-324, 15) === 0.000000000000001

      for precision <- 0..15 do
        assert Float.ceil(-5.0e-324, precision) === -0.0
      end
    end
  end

  describe "round/2" do
    test "with 0.0" do
      for precision <- 0..15 do
        assert Float.round(0.0, precision) === 0.0
        assert Float.round(-0.0, precision) === -0.0
      end
    end

    test "with regular floats" do
      assert Float.round(5.5675, 3) === 5.567
      assert Float.round(-5.5674, 3) === -5.567
      assert Float.round(5.5, 3) === 5.5
      assert Float.round(5.5e-10, 10) === 5.0e-10
      assert Float.round(5.5e-10, 8) === 0.0
      assert Float.round(5.0, 0) === 5.0

      assert_raise ArgumentError, "precision 16 is out of valid range of 0..15", fn ->
        Float.round(1.1, 16)
      end
    end

    test "with subnormal floats" do
      for precision <- 0..15 do
        assert Float.round(5.0e-324, precision) === 0.0
        assert Float.round(-5.0e-324, precision) === -0.0
      end
    end
  end

  describe "ratio/1" do
    test "with 0.0" do
      assert Float.ratio(0.0) == {0, 1}
    end

    test "with regular floats" do
      assert Float.ratio(3.14) == {7_070_651_414_971_679, 2_251_799_813_685_248}
      assert Float.ratio(-3.14) == {-7_070_651_414_971_679, 2_251_799_813_685_248}
      assert Float.ratio(1.5) == {3, 2}
    end

    test "with subnormal floats" do
      assert Float.ratio(5.0e-324) ==
               {1,
                202_402_253_307_310_618_352_495_346_718_917_307_049_556_649_764_142_118_356_901_358_027_430_339_567_995_346_891_960_383_701_437_124_495_187_077_864_316_811_911_389_808_737_385_793_476_867_013_399_940_738_509_921_517_424_276_566_361_364_466_907_742_093_216_341_239_767_678_472_745_068_562_007_483_424_692_698_618_103_355_649_159_556_340_810_056_512_358_769_552_333_414_615_230_502_532_186_327_508_646_006_263_307_707_741_093_494_784}

      assert Float.ratio(1.0e-323) ==
               {1,
                101_201_126_653_655_309_176_247_673_359_458_653_524_778_324_882_071_059_178_450_679_013_715_169_783_997_673_445_980_191_850_718_562_247_593_538_932_158_405_955_694_904_368_692_896_738_433_506_699_970_369_254_960_758_712_138_283_180_682_233_453_871_046_608_170_619_883_839_236_372_534_281_003_741_712_346_349_309_051_677_824_579_778_170_405_028_256_179_384_776_166_707_307_615_251_266_093_163_754_323_003_131_653_853_870_546_747_392}

      assert Float.ratio(2.225073858507201e-308) ==
               {4_503_599_627_370_495,
                202_402_253_307_310_618_352_495_346_718_917_307_049_556_649_764_142_118_356_901_358_027_430_339_567_995_346_891_960_383_701_437_124_495_187_077_864_316_811_911_389_808_737_385_793_476_867_013_399_940_738_509_921_517_424_276_566_361_364_466_907_742_093_216_341_239_767_678_472_745_068_562_007_483_424_692_698_618_103_355_649_159_556_340_810_056_512_358_769_552_333_414_615_230_502_532_186_327_508_646_006_263_307_707_741_093_494_784}
    end
  end

  test "truncate/2" do
    assert 2.11 = Float.truncate(2.11, 2)
    assert 2.11 = Float.truncate(2.112029090909091e-5, 2)
    assert 12.11 = Float.truncate(12.11, 2)
    assert 120.11 = Float.truncate(120.11, 2)
    assert 1200.11 = Float.truncate(1200.11, 2)
    assert 120.0 = Float.truncate(120.00, 2)
    assert 12.345 = Float.truncate(12.3457654, 3)
  end
end
