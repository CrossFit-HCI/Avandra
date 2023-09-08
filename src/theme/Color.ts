interface ComponentColorTheme {
    background: string
}

interface BorderColorTheme {
    primary: string
}

interface ColorTheme {
    component: ComponentColorTheme,
    border: BorderColorTheme
}

export const colorTheme: ColorTheme = {
    component: {
        background: 'white'
    },
    border: {
        primary: 'rgba(0, 0, 0, .2)'
    }
}