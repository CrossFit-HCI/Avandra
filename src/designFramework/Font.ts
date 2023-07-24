interface PrimaryFont {
    color: string,
    family: string,
    size: number,
    style: string,
    weight: string | number
}

interface SecondaryFont {
    color: string,
    family: string,
    size: number,
    style: string,
    weight: string | number
}

interface TertiaryFont {
    color: string,
    family: string,
    size: number,
    style: string,
    weight: string | number
}

interface FontTheme {
    primaryFont: PrimaryFont,
    secondaryFont: SecondaryFont,
    tertiaryFont: TertiaryFont
}

const fontFamily: string = "Arial";
const fontColor: string = "black";

const fontTheme: FontTheme = {
    primaryFont: {
        family: fontFamily,
        color: fontColor,
        size: 0,
        style: "",
        weight: ""
    },
    secondaryFont: {
        family: fontFamily,
        color: fontColor,
        size: 0,
        style: "",
        weight: ""
    },
    tertiaryFont: {
        family: fontFamily,
        color: fontColor,
        size: 0,
        style: "",
        weight: ""
    }
}

export default fontTheme;