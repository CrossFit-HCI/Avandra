export interface TrainingBlock {
    description: string
}

export const mkTrainingBlock = (description: string): TrainingBlock => {
    return {
        description: description
    };
};

export interface Journal {
    title: string,
    trainingBlocks: TrainingBlock[]
}

export const mkJournal = (title: string, trainingBlocks: TrainingBlock[]): Journal => {
    return {
        title: title,
        trainingBlocks: trainingBlocks
    };
};

export const getJournal = (): Journal => {
    return mkJournal('Journal Title', [mkTrainingBlock('Training Block'), mkTrainingBlock('Training Block')]);
};