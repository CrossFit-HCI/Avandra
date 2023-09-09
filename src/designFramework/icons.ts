export interface IconTheme {
    secondarySize: number,
    completed: string,
    uncompleted: string
}

export const iconTheme: IconTheme = {
    secondarySize: 30,
    uncompleted: 'checkbox-blank-circle-outline',
    completed: 'checkbox-marked-circle'
};