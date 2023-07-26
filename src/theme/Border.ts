interface ComponentBorderTheme {
    borderColor: string,
    borderWidth: number,    
}

interface BorderTheme {
    component: ComponentBorderTheme
}

export const borderTheme: BorderTheme = {
    component: {
        borderColor: 'black',
        borderWidth: 2,
    }
}