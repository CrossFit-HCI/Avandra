interface Heading1Font {
    color: string,
    family: string,
    size: number,
    style: string,
    weight: string | number
}

interface Heading2Font {
    color: string,
    family: string,
    size: number,
    style: string,
    weight: string | number
}

interface Heading3Font {
    color: string,
    family: string,
    size: number,
    style: string,
    weight: string | number
}

interface FontTheme {
    heading1Font: Heading1Font,
    heading2Font: Heading2Font,
    heading3Font: Heading3Font
}

const fontFamily: string = 'Arial';
const fontColor: string = 'black';

export const fontTheme: FontTheme = {
    heading1Font: {
        family: fontFamily,
        color: fontColor,
        size: 20,
        style: '',
        weight: ''
    },
    heading2Font: {
        family: fontFamily,
        color: fontColor,
        size: 0,
        style: '',
        weight: ''
    },
    heading3Font: {
        family: fontFamily,
        color: fontColor,
        size: 0,
        style: '',
        weight: ''
    }
};
