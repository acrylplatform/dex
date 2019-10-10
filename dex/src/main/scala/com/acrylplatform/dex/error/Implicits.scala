package com.acrylplatform.dex.error

import cats.syntax.contravariant._
import com.acrylplatform.account.{Address, PublicKey}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.dex.error.ContextShow.{show, auto => autoShow}
import com.acrylplatform.dex.model.MatcherModel.Denormalization
import com.acrylplatform.dex.settings.formatValue
import com.acrylplatform.features.BlockchainFeature
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.assets.exchange.AssetPair
import play.api.libs.json.{JsObject, JsValue, Json, Writes}
import shapeless.ops.hlist.{Mapper, ToList}
import shapeless.{HList, Id, Poly1, ProductArgs}

object Implicits {
  // Here, because we doesn't want to leak this implicits outside the error package

  implicit val byteShow: ContextShow[Byte]                           = autoShow[Byte]
  implicit val intShow: ContextShow[Int]                             = autoShow[Int]
  implicit val longShow: ContextShow[Long]                           = autoShow[Long]
  implicit val stringShow: ContextShow[String]                       = show[String](identity)
  implicit val doubleShow: ContextShow[Double]                       = stringShow.contramap[Double]((x: Double) => formatValue(x))
  implicit val byteStrShow: ContextShow[ByteStr]                     = show[ByteStr](_.base58)
  implicit val assetShow: ContextShow[Asset]                         = show[Asset](AssetPair.assetIdStr)
  implicit val assetPairShow: ContextShow[AssetPair]                 = show[AssetPair](_.key)
  implicit val publicKeyShow: ContextShow[PublicKey]                 = show[PublicKey](_.base58)
  implicit val addressShow: ContextShow[Address]                     = show[Address](_.stringRepr)
  implicit val blockchainFeatureShow: ContextShow[BlockchainFeature] = show[BlockchainFeature](_.description)

  implicit val amountShow: ContextShow[Amount] = new ContextShow[Amount] {
    override def show(input: Amount)(context: ErrorFormatterContext): String = {
      val denormalizedV = Denormalization.denormalizeAmountAndFee(input.volume, context.assetDecimals(input.asset))
      s"${formatValue(denormalizedV)} ${assetShow.show(input.asset)(context)}"
    }
  }

  implicit val priceShow: ContextShow[Price] = new ContextShow[Price] {
    override def show(input: Price)(context: ErrorFormatterContext): String =
      formatValue(
        Denormalization
          .denormalizePrice(
            input.volume,
            context.assetDecimals(input.assetPair.amountAsset),
            context.assetDecimals(input.assetPair.priceAsset)
          ))
  }

  implicit val balanceShow: ContextShow[Map[Asset, Long]] = new ContextShow[Map[Asset, Long]] {
    override def show(input: Map[Asset, Long])(context: ErrorFormatterContext): String =
      input.map(Function.tupled(Amount.apply)).map(amountShow.show(_)(context)).mkString(" and ")
  }

  implicit def setShow[T](implicit itemShow: ContextShow[T]): ContextShow[Set[T]] = new ContextShow[Set[T]] {
    override def show(input: Set[T])(context: ErrorFormatterContext): String = s"${input.map(itemShow.show(_)(context)).mkString(", ")}"
  }

  implicit def listShow[T](implicit itemShow: ContextShow[T]): ContextShow[List[T]] = new ContextShow[List[T]] {
    override def show(input: List[T])(context: ErrorFormatterContext): String = s"${input.map(itemShow.show(_)(context)).mkString(", ")}"
  }

  implicit val booleanWrites: ContextWrites[Boolean]                     = ContextWrites.auto[Boolean]
  implicit val intWrites: ContextWrites[Int]                             = ContextWrites.auto[Int]
  implicit val byteWrites: ContextWrites[Byte]                           = intWrites.contramap[Byte](_.toInt)
  implicit val longWrites: ContextWrites[Long]                           = ContextWrites.auto[Long]
  implicit val doubleWrites: ContextWrites[Double]                       = ContextWrites.auto[String].contramap[Double](formatValue)
  implicit val strWrites: ContextWrites[String]                          = ContextWrites.auto[String]
  implicit val byteStrWrites: ContextWrites[ByteStr]                     = strWrites.contramap[ByteStr](_.base58)
  implicit val assetWrites: ContextWrites[Asset]                         = strWrites.contramap[Asset](AssetPair.assetIdStr)
  implicit val assetPairWrites: ContextWrites[AssetPair]                 = ContextWrites.contextWrites[AssetPair]((x, _) => x.json)
  implicit val publicKeyWrites: ContextWrites[PublicKey]                 = strWrites.contramap[PublicKey](_.base58)
  implicit val addressWrites: ContextWrites[Address]                     = strWrites.contramap[Address](_.stringRepr)
  implicit val blockchainFeatureWrites: ContextWrites[BlockchainFeature] = strWrites.contramap[BlockchainFeature](_.description)

  implicit val amountWrites: ContextWrites[Amount] = new ContextWrites[Amount] {
    override def writes(input: Amount)(context: ErrorFormatterContext): JsValue = {
      val denormalizedV = Denormalization.denormalizeAmountAndFee(input.volume, context.assetDecimals(input.asset))
      Json.obj(
        "volume"  -> doubleWrites.writes(denormalizedV)(context),
        "assetId" -> assetWrites.writes(input.asset)(context)
      )
    }
  }

  implicit val balanceWrites: ContextWrites[Map[Asset, Long]] = new ContextWrites[Map[Asset, Long]] {
    override def writes(input: Map[Asset, Long])(context: ErrorFormatterContext): JsValue = {
      val xs = input.map {
        case (k, v) =>
          val denormalizedV = Denormalization.denormalizeAmountAndFee(v, context.assetDecimals(k))
          assetShow.show(k)(context) -> doubleWrites.writes(denormalizedV)(context)
      }
      JsObject(xs)
    }
  }

  implicit val priceWrites: ContextWrites[Price] = new ContextWrites[Price] {
    override def writes(input: Price)(context: ErrorFormatterContext): JsValue =
      doubleWrites.writes(
        Denormalization
          .denormalizePrice(
            input.volume,
            context.assetDecimals(input.assetPair.amountAsset),
            context.assetDecimals(input.assetPair.priceAsset)
          )
      )(context)
  }

  implicit def setWrites[T](implicit itemWrites: ContextWrites[T]): ContextWrites[Set[T]] = new ContextWrites[Set[T]] {
    override def writes(input: Set[T])(context: ErrorFormatterContext): JsValue = {
      val xs = input.map(itemWrites.writes(_)(context))
      implicitly[Writes[Set[JsValue]]].writes(xs)
    }
  }

  implicit def listWrites[T](implicit itemWrites: ContextWrites[T]): ContextWrites[List[T]] = new ContextWrites[List[T]] {
    override def writes(input: List[T])(context: ErrorFormatterContext): JsValue = {
      val xs = input.map(itemWrites.writes(_)(context))
      implicitly[Writes[List[JsValue]]].writes(xs)
    }
  }

  implicit class ErrorInterpolator(sc: StringContext) {
    class Args extends ProductArgs {
      def applyProduct[H <: HList, L <: HList](args: H)(
          implicit
          formatArgs: Mapper.Aux[FormatArg.type, H, L],
          toList: ToList[L, Id[_]]
      ): ErrorFormatterContext => MatcherErrorMessage = {
        val (nameArgs, strArgsFn, jsonArgsFn) = toList(formatArgs(args))
          .asInstanceOf[List[(String, ErrorFormatterContext => String, ErrorFormatterContext => JsValue)]]
          .unzip3

        val parts = sc.parts.init

        { context =>
          val (message, template, params) = parts.zipWithIndex.foldLeft(("", "", JsObject.empty)) {
            case ((m, t, p), (x, i)) =>
              val name = nameArgs(i)
              val str  = strArgsFn(i)(context)
              val json = jsonArgsFn(i)(context)
              (s"$m$x$str", s"$t$x{{$name}}", p + (name -> json))
          }
          MatcherErrorMessage(
            normalize(message + sc.parts.last),
            normalize(template + sc.parts.last),
            params
          )
        }
      }
    }

    val e: Args = new Args

    def normalize(x: String): String = x.stripMargin('|').replaceAll("\n", " ").trim
  }

  object FormatArg extends Poly1 {
    implicit def mapAt[T](
        implicit show: ContextShow[T],
        json: ContextWrites[T]): Case.Aux[(Symbol, T), (String, ErrorFormatterContext => String, ErrorFormatterContext => JsValue)] =
      at[(Symbol, T)] {
        case (name, x) => (name.name, show.show(x), json.writes(x))
      }
  }
}
