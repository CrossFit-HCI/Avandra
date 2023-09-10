interface ComponentColor {
    background: string
}

interface BorderColor {
    primary: string
}

interface ButtonColor {
    primaryBackground: string,
    secondaryBackground?: string,
    primaryFont: string,
    secondaryFont?: string
}

interface ColorDF {
    component: ComponentColor,
    border: BorderColor,
    button: ButtonColor
}

export const colorDF: ColorDF = {
    component: {
        background: 'white'
    },
    border: {
        primary: 'rgba(0, 0, 0, .2)'
    },
    button: {
        primaryBackground: 'black',
        primaryFont: 'white'
    }
};