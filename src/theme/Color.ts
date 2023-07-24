interface ComponentColorTheme {
    background: string
}

interface ColorTheme {
    component: ComponentColorTheme
}

export const colorTheme: ColorTheme = {
    component: {
        background: 'white'
    }
}