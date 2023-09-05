interface ComponentBorderTheme {
    borderColor: string,
    borderWidth: number,    
}

interface BorderTheme {
    component: ComponentBorderTheme,
    card: ComponentBorderTheme
}

export const borderTheme: BorderTheme = {
    component: {
        borderColor: 'black',
        borderWidth: 0.3,
    },
    card: {
        borderColor: 'rgba(0, 0, 0, .2)',
        borderWidth: 0.3
    }
}